package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.dto.CategoryProductAttributeExtractedDTO;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.outbound.matrix.MatrixOutbound;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import com.gdn.x.productcategorybase.repository.ProductAttributeExtractedRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductAttributeExtractionServiceImpl implements ProductAttributeExtractionService {

  @Autowired
  private ProductAttributeExtractedRepository productAttributeExtractedRepository;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private MatrixOutbound matrixOutbound;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  @Lazy
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  @Transactional(readOnly = false)
  public List<CategoryProductAttributeExtractedDTO> publishProductsForBackfillingByStatus(String storeId, String username, String status) {
    int maxPageNumber = Integer.parseInt(
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER)
            .getValue());
    int maxPageSize = Integer.parseInt(
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE)
            .getValue());
    String sortOrder =
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION)
            .getValue();
    return processPendingProducts(storeId, username, maxPageNumber, maxPageSize, sortOrder, status);
  }

  private List<CategoryProductAttributeExtractedDTO> processPendingProducts(String storeId, String username, int maxPageNumber, int maxPageSize,
      String sortOrder, String status) {
    log.info("Starting with attribute extraction batch. maxPageNumber : {}, maxPageSize : {}", maxPageNumber,
        maxPageSize);
    long startTime = System.currentTimeMillis();
    List<ProductAttributeExtracted> updatedProductEttributeExtractedList = new ArrayList<>();
    Map<String, Category> categoryMap = new HashMap<>();
    for (int page = 0; page < maxPageNumber; page++) {
      Pageable pageRequest = PageRequest.of(page, maxPageSize,
          Sort.by(Sort.Direction.DESC, Constants.UPDATED_DATE).and(Sort.by(Sort.Direction.ASC, Constants.ID)));
      if (Constants.ASC_SORT_ORDER.equals(sortOrder)) {
        pageRequest = PageRequest.of(page, maxPageSize,
            Sort.by(Sort.Direction.ASC, Constants.UPDATED_DATE).and(Sort.by(Sort.Direction.ASC, Constants.ID)));
      }
      Page<ProductAttributeExtracted> productAttributeExtractedPage = productAttributeExtractedRepository
          .findByStoreIdAndStatusAndMarkForDeleteFalse(storeId, ExtractionStatus.valueOf(status), pageRequest);
      if (CollectionUtils.isNotEmpty(productAttributeExtractedPage.getContent())) {
        try {
          List<Category> categories = categoryService.findByStoreIdAndCategoryCodes(storeId,
              productAttributeExtractedPage.getContent().stream().map(ProductAttributeExtracted::getCnCategoryCode)
                  .distinct().collect(Collectors.toList()));
          categories.forEach(category -> categoryMap.put(category.getCategoryCode(), category));
          updateStatusForBackfilling(username, categoryMap, productAttributeExtractedPage.getContent(),
              updatedProductEttributeExtractedList);
        } catch (ApplicationRuntimeException e) {
          // handle the runtime exception and proceed to next batch.
          log.error("Error caught while processing page : {}, size : {} ", page, maxPageSize, e);
        }
      } else {
        log.info("No pending records found for attribute extraction. storeId : {}, page : {}, size : {} ", storeId,
            page, maxPageSize);
      }
    }
    List<CategoryProductAttributeExtractedDTO> categoryProductAttributeExtractedList =
      saveAndPublishEventForBackfilling(storeId,
      username, updatedProductEttributeExtractedList, categoryMap);
    long endTime = System.currentTimeMillis();
    log.info("Finished with attribute extraction batch processing. totalTimeTaken : {}", (endTime - startTime));
    return categoryProductAttributeExtractedList;
  }

  private void updateStatusForBackfilling(String username, Map<String, Category> categoryMap,
      List<ProductAttributeExtracted> productAttributeExtractedList,
      List<ProductAttributeExtracted> updatedProductAttributeExtractedList) {
    log.info("Starting the publish of backfilling event. fetchedRecords : {} ", productAttributeExtractedList.size());
    long startTime = System.currentTimeMillis();
    for (ProductAttributeExtracted productAttributeExtracted : productAttributeExtractedList) {
      Category category = categoryMap.get(productAttributeExtracted.getCnCategoryCode());
      if (Objects.nonNull(category) && Objects.nonNull(category.getExtractionType())) {
        updatedProductAttributeExtractedList.add(productAttributeExtracted);
      } else {
        log.info(
            "Category not found for that categoryCode or extractionType is NULL.  productAttributeExtracted : {}, category : {}",
            productAttributeExtracted, category);
      }
    }
    long endTime = System.currentTimeMillis();
    log.info("Finshed the publishing of backfilling event. totalTimeTake : {}", (endTime - startTime));
  }

  private List<CategoryProductAttributeExtractedDTO> saveAndPublishEventForBackfilling(String storeId, String username,
      List<ProductAttributeExtracted> updatedProductAttributeExtractedList, Map<String, Category> categoryMap) {
    List<CategoryProductAttributeExtractedDTO> categoryAndProductAttributeExtractedList = new ArrayList<>();
    for (ProductAttributeExtracted productAttributeExtracted : updatedProductAttributeExtractedList) {
      Category category = categoryMap.get(productAttributeExtracted.getCnCategoryCode());
      productAttributeExtracted.setStatus(ExtractionStatus.IN_PROGRESS);
      productAttributeExtracted.setUpdatedBy(username);
      ProductAttributeExtracted updatedProductAttributeExtracted =
          productAttributeExtractedRepository.save(productAttributeExtracted);
      categoryAndProductAttributeExtractedList.add(
        CategoryProductAttributeExtractedDTO.builder().category(category)
          .productAttributeExtracted(updatedProductAttributeExtracted).build());
    }
    return categoryAndProductAttributeExtractedList;
  }

  public void publishAttributeExtractionEvent(String storeId, String username, Category category,
    ProductAttributeExtracted updatedProductAttributeExtracted) {
    domainEventPublisherService.publishProductAttributeExtractionBackfillingEvent(storeId, username,
        updatedProductAttributeExtracted.getProductCode(), updatedProductAttributeExtracted.getCnCategoryCode(),
        category.getName(), category.getExtractionType().name());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateExtractedAttributesForBackfilling(ProductAttributeExtractionModel productAttributeExtractionModel) {
    ProductAttributeExtracted productAttributeExtracted = productAttributeExtractedRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(productAttributeExtractionModel.getStoreId(),
            productAttributeExtractionModel.getProductCode());
    try {
      Product product = productService
          .getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(productAttributeExtractionModel.getStoreId(),
              productAttributeExtractionModel.getProductCode());
      if (ExtractionType.TEXT.name().equals(productAttributeExtractionModel.getExtractionType())) {
        extractAttributeBasedOnTextAndBackFill(product, productAttributeExtractionModel, productAttributeExtracted);
      } else if (ExtractionType.INLINE_TEXT.name().equals(productAttributeExtractionModel.getExtractionType())) {
        extractAttributeBasedOnInlineTextAndBackFill(productAttributeExtracted);
      } else if (ExtractionType.TEXT_IMAGE.name().equals(productAttributeExtractionModel.getExtractionType())) {
        // Will be picked up future. Right now its only based on text details.
      }
    } catch (Exception e) {
      log.error("Error caught while extracting attribute. productAttributeExtractionModel : {} ",
          productAttributeExtractionModel, e);
      productAttributeExtracted.setStatus(ExtractionStatus.FAILED);
      productAttributeExtracted.setErrorMessage(ErrorMessage.SYSTEM_ERROR.getMessage());
    }
    productAttributeExtracted.setSource(Constants.MATRIX);
    productAttributeExtracted.setUpdatedBy(productAttributeExtractionModel.getUsername());
    productAttributeExtractedRepository.save(productAttributeExtracted);
  }

  private void extractAttributeBasedOnInlineTextAndBackFill(
    ProductAttributeExtracted productAttributeExtracted) throws Exception {
    productServiceWrapper.autoFillAttributes(productAttributeExtracted.getStoreId(),
      productAttributeExtracted.getProductCode());
    productAttributeExtracted.setStatus(ExtractionStatus.SUCCESS);
  }

  private void extractAttributeBasedOnTextAndBackFill(Product product,
      ProductAttributeExtractionModel productAttributeExtractionModel,
      ProductAttributeExtracted productAttributeExtracted) throws JsonProcessingException {
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse = matrixOutbound
        .extractProductAttributesByTextDetails(
            ConverterUtil.toMatrixAttributeExtractionRequest(product, productAttributeExtractionModel));
    if (!matrixAttributeExtractionResponse.isSuccess()) {
      productAttributeExtracted.setStatus(ExtractionStatus.FAILED);
      productAttributeExtracted.setErrorMessage(
          new StringBuilder(matrixAttributeExtractionResponse.getErrorCode()).append(Constants.HYPHEN)
              .append(matrixAttributeExtractionResponse.getErrorMessage()).toString());
    } else {
      productAttributeExtracted.setExtractedAttribute(
          objectMapper.writeValueAsString(matrixAttributeExtractionResponse.getValue().getExtractedAttributes()));
      productAttributeExtracted.setStatus(ExtractionStatus.SUCCESS);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void addProductsToProductAttributeExtraction(String productCode, Category category) {
    if (Objects.nonNull(category.getExtractionType()) && !ExtractionType.INLINE_TEXT.equals(category.getExtractionType())) {
      ProductAttributeExtracted productAttributeExtracted = productAttributeExtractedRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, productCode);
      if (Objects.nonNull(productAttributeExtracted)) {
        productAttributeExtracted.setProductCode(productCode);
        productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
        productAttributeExtracted.setCnCategoryCode(category.getCategoryCode());
      } else {
        productAttributeExtracted = ConverterUtil.getProductAttributeExtracted(productCode, category.getCategoryCode());
      }
      productAttributeExtractedRepository.save(productAttributeExtracted);
    }
  }

  @Override
  public ProductAttributeExtracted fetchAndSetProductAttributeExtraction(String productCode, Category category,
    String storeId) {
    if (Objects.nonNull(category.getExtractionType()) && !ExtractionType.INLINE_TEXT.equals(category.getExtractionType())) {
      ProductAttributeExtracted productAttributeExtracted = productAttributeExtractedRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, productCode);
      if (Objects.nonNull(productAttributeExtracted)) {
        productAttributeExtracted.setProductCode(productCode);
        productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
        productAttributeExtracted.setCnCategoryCode(category.getCategoryCode());
      } else {
        productAttributeExtracted = ConverterUtil.getProductAttributeExtracted(productCode, category.getCategoryCode());
      }
      return productAttributeExtracted;
    }
    return null;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveProductToAttributeExtraction(
    ProductAttributeExtracted productAttributeExtracted) {
    if(Objects.nonNull(productAttributeExtracted)) {
      this.productAttributeExtractedRepository.saveAndFlush(productAttributeExtracted);
    }
  }

}


