package com.gdn.x.mta.distributiontask.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductAutoApprovalRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.dto.AutoQcChangedFieldDto;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.enums.AutoQcConfigChangeStatus;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.response.AutoQcConfigChangeDto;
import com.gdn.x.mta.distributiontask.service.api.AutoQcConfigChangeService;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductAutoApprovalServiceImpl implements ProductAutoApprovalService {

  private static final String PRODUCT_CODE_MUST_NOT_BE_EMPTY = "Product code list should not be empty";

  @Autowired
  private ProductAutoApprovalRepository productAutoApprovalRepository;

  @Lazy
  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private AutoQcConfigChangeService autoQcConfigChangeService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void addProductsToAutoApprovalTable(String storeId, List<String> productCodes,
    Map<String, String> productCodeXCategoryCode) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes), PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    List<ProductAutoApproval> productAutoApprovalList = new ArrayList<>();
    String categoryCode = null;
    List<ProductAutoApproval> productApprovals =
        productAutoApprovalRepository.findByStoreIdAndProductCodeIn(storeId, productCodes);
    Map<String, ProductAutoApproval> productAutoApprovals =
        productApprovals.stream().collect(Collectors.toMap(ProductAutoApproval::getProductCode, Function.identity()));
    for (String productCode : productCodes) {
      ProductAutoApproval productAutoApproval;
      if(MapUtils.isNotEmpty(productCodeXCategoryCode)){
        categoryCode = productCodeXCategoryCode.get(productCode);
      }
      if (productAutoApprovals.containsKey(productCode)) {
        productAutoApproval = productAutoApprovals.get(productCode);
        productAutoApproval.setCategoryCode(categoryCode);
        if(!AutoApprovalStatus.PUBLISHED.equals(productAutoApproval.getAutoApprovalStatus())) {
          productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.PENDING);
          productAutoApproval.setRetryCount(0);
          productAutoApproval.setMarkForDelete(false);
          productAutoApprovalList.add(productAutoApproval);
        } else {
          MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.SYSTEM);
          SystemParameterConfig maxRetryCountForAutoApprovalRetry = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
              Constants.MAX_RETRY_COUNT_FOR_AUTO_APPROVAL_RETRY);
          int maxRetryCount = Integer.parseInt(maxRetryCountForAutoApprovalRetry.getValue());
          if(productAutoApproval.getRetryCount() <= maxRetryCount) {
            productAutoApproval.setMarkForDelete(false);
            productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.PENDING);
            productAutoApproval.setRetryCount(productAutoApproval.getRetryCount()+1);
            productAutoApprovalList.add(productAutoApproval);
          } else {
            productAutoApproval.setMarkForDelete(Boolean.TRUE);
            productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.FAILED);
            productAutoApproval.setUpdatedDate(new Date());
            productAutoApprovalList.add(productAutoApproval);
          }
        }
      } else {
        productAutoApproval = getProductAutoApproval(productCode, AutoApprovalStatus.PENDING);
        productAutoApproval.setCategoryCode(categoryCode);
        productAutoApprovalList.add(productAutoApproval);
      }
    }
    productAutoApprovalRepository.saveAll(productAutoApprovalList);
  }

  private ProductAutoApproval getProductAutoApproval(String productCode, AutoApprovalStatus status) {
    ProductAutoApproval productAutoApproval = new ProductAutoApproval();
    productAutoApproval.setProductCode(productCode);
    productAutoApproval.setCreatedBy(Constants.SYSTEM);
    productAutoApproval.setStoreId(Constants.DEFAULT_STORE_ID);
    productAutoApproval.setUpdatedBy(Constants.SYSTEM);
    productAutoApproval.setRetryCount(0);
    productAutoApproval.setAutoApprovalStatus(status);
    return productAutoApproval;
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAutoApproval> findProductsToAutoApprovalOrderByCreatedDateAsc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals) {
    List<ProductAutoApproval> productForAutoApprovals = productAutoApprovalRepository
        .findByStoreIdAndAutoApprovalStatusOrderByCreatedDateAsc(storeId, autoApprovalStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductAutoApprovals));
    return productForAutoApprovals;
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAutoApproval> findProductsToAutoApprovalOrderByCreatedDateDesc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals) {
    List<ProductAutoApproval> productForAutoApprovals = productAutoApprovalRepository
        .findByStoreIdAndAutoApprovalStatusOrderByCreatedDateDesc(storeId, autoApprovalStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductAutoApprovals));
    return productForAutoApprovals;
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAutoApproval> findProductsToAutoApprovalOrderByUpdatedDateAsc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals) {
    List<ProductAutoApproval> productForAutoApprovals = productAutoApprovalRepository
        .findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateAsc(storeId, autoApprovalStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductAutoApprovals));
    return productForAutoApprovals;
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAutoApproval> findProductsToAutoApprovalOrderByUpdatedDateDesc(String storeId,
      AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovals) {
    List<ProductAutoApproval> productForAutoApprovals = productAutoApprovalRepository
        .findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateDesc(storeId, autoApprovalStatus,
            PageRequest.of(Constants.PAGE_ZERO, maximumAllowedProductAutoApprovals));
    return productForAutoApprovals;
  }



  @Override
  @Transactional(readOnly = false)
  public void updateProductAutoApprovalDetails(ProductAutoApproval productAutoApproval) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.SYSTEM);
    if (productAutoApproval.getAutoApprovalStatus().equals(AutoApprovalStatus.NA)
        || productAutoApproval.getAutoApprovalStatus().equals(AutoApprovalStatus.FAILED)) {
      productAutoApproval.setMarkForDelete(Boolean.TRUE);
    }
    productAutoApproval.setUpdatedDate(new Date());
    productAutoApprovalRepository.save(productAutoApproval);
  }

  @Override
  @Transactional(readOnly = false)
  public void updateProductAutoApprovalDetailsByProductCode(String storeId,
      String productCode, AutoApprovalStatus autoApprovalStatus, boolean isResetCategory) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.SYSTEM);
    ProductAutoApproval productAutoApproval =
        productAutoApprovalRepository.findByStoreIdAndProductCode(storeId, productCode);
    productAutoApproval.setMarkForDelete(Boolean.TRUE);
    productAutoApproval.setAutoApprovalStatus(autoApprovalStatus);
    productAutoApproval.setUpdatedDate(new Date());
    if(isResetCategory){
      productAutoApproval.setCategoryCode(StringUtils.EMPTY);
    }
    productAutoApprovalRepository.save(productAutoApproval);
  }

  @Override
  public void processPendingAutoQcConfigChange(String storeId) {
    int autoQcConfigChangeFetchBatchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE).getValue());
    List<AutoQcConfigChange> autoQcConfigChangeList =
        autoQcConfigChangeService.fetchAutoQcConfigChangesByStatus(storeId, AutoQcConfigChangeStatus.PENDING.name(),
            autoQcConfigChangeFetchBatchSize);
    if (CollectionUtils.isNotEmpty(autoQcConfigChangeList)) {
      autoQcConfigChangeList =
          updateAutoQcConfigChangeStatus(autoQcConfigChangeList, AutoQcConfigChangeStatus.IN_PROGRESS.name());
      validateConfigChangeAndUpdateAutoApproval(storeId, autoQcConfigChangeList);
    }
  }

  private void validateConfigChangeAndUpdateAutoApproval(String storeId,
      List<AutoQcConfigChange> autoQcConfigChangeList) {
    for (AutoQcConfigChange autoQcConfigChange : autoQcConfigChangeList) {
      try {
        if (productServiceRepository.validateAutoQcConfigChange(
            ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChange))) {
          CategoryCodeResponse categoryCodeResponse = productServiceRepository.getCnCategoryCodesFromC1(
              new CategoryCodeRequest(Arrays.asList(autoQcConfigChange.getC1CategoryCode())));
          addProductsForApproval(storeId, autoQcConfigChange,
              categoryCodeResponse.getCategoryCodes().stream().collect(Collectors.toSet()));
        }
        updateAutoQcConfigChangeStatus(autoQcConfigChange, AutoQcConfigChangeStatus.SUCCESS.name());
      } catch (Exception e) {
        log.error("Exception caught while adding product to auto approval on config change. {} ", e);
        updateAutoQcConfigChangeStatus(autoQcConfigChange, AutoQcConfigChangeStatus.FAILED.name());
      }
    }
  }

  private void addProductsForApproval(String storeId, AutoQcConfigChange autoQcConfigChange,
      Set<String> cnCategoryCodes) {
    int productApprovalInsertBatchSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            Constants.PRODUCT_AUTO_APPROVAL_INSERT_BATCH_SIZE).getValue());
    int page = 0;
    long totalProducts = 0;
    Pageable pageable = PageRequest.of(page, productApprovalInsertBatchSize,
        Sort.by(new Sort.Order(Sort.Direction.ASC, Constants.CREATED_DATE)));
    do {
      Page<Product> productPage =
          productService.getProductsBySellerCodeAndCategoryCodes(storeId, autoQcConfigChange.getSellerCode(),
              WorkflowState.IN_REVIEW, cnCategoryCodes, pageable);
      totalProducts = productPage.getTotalElements();
      if (CollectionUtils.isNotEmpty(productPage.getContent())) {
        this.addProductsToAutoApprovalTable(storeId,
            productPage.getContent().stream().map(Product::getProductCode).collect(Collectors.toList()), Collections.emptyMap());
      }
      page++;
    } while (page * productApprovalInsertBatchSize < totalProducts);
  }

  private List<AutoQcConfigChange> updateAutoQcConfigChangeStatus(List<AutoQcConfigChange> autoQcConfigChangeList, String status) {
    autoQcConfigChangeList.forEach(autoQcConfigChange -> autoQcConfigChange.setStatus(status));
    return autoQcConfigChangeService.saveAutoQcConfigChanges(autoQcConfigChangeList);
  }

  private void updateAutoQcConfigChangeStatus(AutoQcConfigChange autoQcConfigChange, String status) {
    autoQcConfigChange.setStatus(status);
    autoQcConfigChange = autoQcConfigChangeService.saveAutoQcConfigChange(autoQcConfigChange);
  }

  @Override
  @Transactional(readOnly = false)
  public void saveAutoQcConfigChanges(AutoQcConfigChangeDto autoQcConfigChangeDto) throws IOException {
    AutoQcConfigChange autoQcConfigChange = autoQcConfigChangeService
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    if (Objects.isNull(autoQcConfigChange)) {
      AutoQcConfigChange autoQcConfigChange1 = setAutoQcConfigChange(autoQcConfigChangeDto);
      log.info("Saving new entry in autoQcConfigChange : {} ", autoQcConfigChange1);
      autoQcConfigChangeService.saveAutoQcConfigChanges(Collections.singletonList(autoQcConfigChange1));
    } else {
      updateExistingAutoQcConfig(autoQcConfigChangeDto, autoQcConfigChange);
    }
  }

  private void updateExistingAutoQcConfig(AutoQcConfigChangeDto autoQcConfigChangeDto,
      AutoQcConfigChange autoQcConfigChange) throws IOException {
    if (MapUtils.isNotEmpty(autoQcConfigChangeDto.getChangeFieldResponseMap())) {
      if (StringUtils.isNotBlank(autoQcConfigChange.getChangedFields())) {
        updateChangedFieldDto(autoQcConfigChangeDto, autoQcConfigChange);
      } else {
        autoQcConfigChange
            .setChangedFields(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
      }
    }
    log.info("Updating existing entry in autoQcConfigChange : {} ", autoQcConfigChange);
    autoQcConfigChangeService.saveAutoQcConfigChanges(Collections.singletonList(autoQcConfigChange));
  }

  private void updateChangedFieldDto(AutoQcConfigChangeDto autoQcConfigChangeDto, AutoQcConfigChange autoQcConfigChange)
      throws IOException {
    Map<String, ChangedFieldDto> autoQcChangedFieldDtoMap = objectMapper
        .readValue(autoQcConfigChange.getChangedFields(), new TypeReference<HashMap<String, ChangedFieldDto>>() {
        });
    getChangedFieldDto(autoQcConfigChangeDto, autoQcChangedFieldDtoMap);
    autoQcConfigChange.setChangedFields(objectMapper.writeValueAsString(autoQcChangedFieldDtoMap));
  }

  private void getChangedFieldDto(AutoQcConfigChangeDto autoQcConfigChangeDto,
      Map<String, ChangedFieldDto> autoQcChangedFieldDtoMap) {
    for (Map.Entry<String, AutoQcChangedFieldDto> autoQcChangedFieldDtoEntry : autoQcConfigChangeDto
        .getChangeFieldResponseMap().entrySet()) {
      if (autoQcChangedFieldDtoMap.containsKey(autoQcChangedFieldDtoEntry.getKey())) {
        ChangedFieldDto changedFieldDto = autoQcChangedFieldDtoMap.get(autoQcChangedFieldDtoEntry.getKey());
        changedFieldDto.setNewValue(autoQcChangedFieldDtoEntry.getValue().getNewValue());
        autoQcChangedFieldDtoMap.put(autoQcChangedFieldDtoEntry.getKey(), changedFieldDto);
      } else {
        autoQcChangedFieldDtoMap.put(autoQcChangedFieldDtoEntry.getKey(),
            new ChangedFieldDto(autoQcChangedFieldDtoEntry.getValue().getOldValue(),
                autoQcChangedFieldDtoEntry.getValue().getNewValue()));
      }
    }
  }

  private AutoQcConfigChange setAutoQcConfigChange(AutoQcConfigChangeDto autoQcConfigChangeDto)
      throws JsonProcessingException {
    AutoQcConfigChange autoQcConfigChange1 = new AutoQcConfigChange();
    autoQcConfigChange1.setSellerCode(autoQcConfigChangeDto.getSellerCode());
    autoQcConfigChange1.setC1CategoryCode(autoQcConfigChangeDto.getCategoryCode());
    autoQcConfigChange1.setNewSeller(autoQcConfigChangeDto.isNewData());
    autoQcConfigChange1.setStatus(AutoQcConfigChangeStatus.PENDING.name());
    if(Objects.nonNull(autoQcConfigChangeDto.getChangeFieldResponseMap())) {
      autoQcConfigChange1.setChangedFields(
          objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    }
    autoQcConfigChange1.setStoreId(Constants.DEFAULT_STORE_ID);
    return autoQcConfigChange1;
  }
}
