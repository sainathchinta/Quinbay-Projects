package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SizeChartService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class SizeChartServiceImpl implements SizeChartService {

  private static final String SIZE_CHART_DETAIL_CACHE_KEY = "#storeId + '-' + #sizeChartCode";
  private static final String SIZE_CHART_ADDITION_HISTORY = "Size Chart has been Added";
  private static final String SIZE_CHART_REMOVAL_HISTORY = "Size Chart has been Deleted";

  @Autowired
  private ProductService productService;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private KafkaPublisher kafkaPublisher;


  @Override
  @Cacheable(cacheManager = Constants.SIZE_CHART_DETAIL_CACHE_MANAGER, value = CacheNames.SIZE_CHART_DETAIL, key = SIZE_CHART_DETAIL_CACHE_KEY, unless = "#result == null")
  public SizeChartResponse fetchSizeChartDetails(String storeId, String sizeChartCode)
      throws ApplicationRuntimeException {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(sizeChartCode),
        ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_NULL);
    return productCategoryBaseOutbound.fetchSizeChartDetails(storeId, sizeChartCode);
  }

  @Override
  public void updateProductSizeChartCode(String storeId, String sizeChartCode,
      ProductSizeChartUpdateRequest productSizeChartUpdateRequest) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(sizeChartCode),
        ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_NULL);

    Set<String> addInSkus =
        Optional.ofNullable(productSizeChartUpdateRequest.getAddSizeChartSkus()).orElse(new HashSet<>());
    Set<String> removeFromSkus =
        Optional.ofNullable(productSizeChartUpdateRequest.getRemoveSizeChartSkus()).orElse(new HashSet<>());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(addInSkus) || CollectionUtils.isNotEmpty(removeFromSkus),
        ErrorMessages.BOTH_ADD_SKUS_AND_REMOVE_SKUS_IS_EMPTY_FOR_SIZE_CHART_UPDATE);

    // validate if any common sku present in add and removal list
    validateForCommonSkuInAddAndRemoveSizeChartRequest(addInSkus, removeFromSkus);

    //validate if size chart is valid
    validateForSizeChartCode(sizeChartCode);

    //fetch products by product skus
    Map<String, Product> productsMap = getProductByProductSkus(storeId, addInSkus, removeFromSkus);

    //validate count of fetched products with count of product skus in request
    validateAddAndRemovalProductCount(productsMap, addInSkus, removeFromSkus);

    //validate if same size chart code is present in product where we are removing the size chart code
    validateForSizeChartCodePresentInProductForRemoval(productsMap, removeFromSkus, sizeChartCode);

    // update size chart code value in product
    AuditTrailListResponse auditTrailListResponse =
        upsertSizeChartCode(productsMap, addInSkus, sizeChartCode);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    auditTrailListResponse.getAuditTrailResponseList()
        .addAll(removeSizeChartCode(productsMap, removeFromSkus, sizeChartCode));

    saveProductAndUpdateInSolr(productsMap);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);

  }

  @Override
  @CacheEvict(cacheManager = Constants.SIZE_CHART_DETAIL_CACHE_MANAGER, value =
      CacheNames.SIZE_CHART_DETAIL, key = SIZE_CHART_DETAIL_CACHE_KEY)
  public void evictSizeChartCache(String storeId, String sizeChartCode) {
    // Evict size chart detail cached for PDP once size chart is updated
    // Event published from PCB to evict the cache
  }

  private void saveProductAndUpdateInSolr(Map<String, Product> productsMap) {
    Collection<Product> updatedProducts = productsMap.values();
    for (Product product : updatedProducts) {
      saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(),
          new ProductAndItemEventModel(product.getProductSku(),
              Map.of(SolrFieldNames.SIZE_CHART_CODE, Map.of(SolrConstants.SET_CLAUSE, product.getSizeChartCode())),
              product.getMerchantCode()));
    }
  }

  private void validateForCommonSkuInAddAndRemoveSizeChartRequest(Set<String> addInSkus, Set<String> removeFromSkus) {
    Set<String> intersection = Sets.intersection(addInSkus, removeFromSkus);
    if (CollectionUtils.isNotEmpty(intersection)) {
      log.error(
          "Common products present in both add and removal list of size chart addInSkus : {} , removeFromSkus : {} ",
          addInSkus, removeFromSkus);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.PRODUCT_PRESENT_IN_BOTH_ADD_AND_REMOVAL_LIST_OF_SIZE_CHART);
    }
  }

  private void validateForSizeChartCode(String sizeChartCode) {
    boolean isValid = productCategoryBaseOutbound.isSizeChartCodeValid(sizeChartCode);
    if (!isValid) {
      log.error("Invalid size chart code : {} ", sizeChartCode);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(ErrorMessages.INVALID_SIZE_CHART_CODE, sizeChartCode));
    }
  }

  private void validateAddAndRemovalProductCount(Map<String, Product> productsMap, Set<String> addInSkus,
      Set<String> removeFromSkus) {
    if (productsMap.size() != addInSkus.size() + removeFromSkus.size()) {
      log.error(
          "Not all products in size chart update request exists addInSkus : {} , removeFromSkus : {} , products : {}",
          addInSkus, removeFromSkus, productsMap.keySet());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.NOT_ALL_PRODUCTS_IN_SIZE_CHART_UPDATE_REQUEST_EXISTS);
    }
  }

  private void validateForSizeChartCodePresentInProductForRemoval(Map<String, Product> productsMap,
      Set<String> removeFromSkus, String sizeChartCode) {
    for (String removeFromSku : removeFromSkus) {
      if (productsMap.containsKey(removeFromSku)) {
        Product product = productsMap.get(removeFromSku);
        if (!sizeChartCode.equals(product.getSizeChartCode())) {
          log.error("Different size chart code present for the product : {} , sizeChartCode : {} ", product,
              sizeChartCode);
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.DIFFERENT_SIZE_CHART_CODE_PRESENT_IN_PRODUCT);
        }
      } else {
        log.error("Product not found : {} ", removeFromSku);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND_ERROR);
      }
    }
  }

  private Map<String, Product> getProductByProductSkus(String storeId, Set<String> addInSkus,
      Set<String> removeFromSkus) throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.addAll(addInSkus);
    productSkus.addAll(removeFromSkus);

    List<Product> products = productService.getProducts(storeId, productSkus);

    return Optional.ofNullable(products).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(Product::getProductSku, Function.identity(), (v1, v2) -> v2));
  }

  private AuditTrailListResponse upsertSizeChartCode(Map<String, Product> productsMap,
      Set<String> addInSkus, String sizeChartCode) {
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setChangedBy(GdnMandatoryRequestParameterUtil.getUsername());
    for (String addInSku : addInSkus) {
      if (productsMap.containsKey(addInSku)) {
        Product product = productsMap.get(addInSku);
        String oldValue = product.getSizeChartCode();
        product.setSizeChartCode(sizeChartCode);
        AuditTrailDto auditTrailDto =
            getAuditTrailDto(sizeChartCode, oldValue,
                product, SIZE_CHART_ADDITION_HISTORY);
        auditTrailListResponse.getAuditTrailResponseList().add(auditTrailDto);
      } else {
        log.error("Product not found : {} ", addInSku);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND_ERROR);
      }
    }
    return auditTrailListResponse;
  }

  private AuditTrailDto getAuditTrailDto(String newValue, String oldValue,
      Product product, String activity) {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setNewValue(newValue);
    auditTrailDto.setOldValue(Optional.ofNullable(oldValue).orElse(Constants.HYPHEN));
    auditTrailDto.setProductSku(product.getProductSku());
    auditTrailDto.setActionKey(activity);
    auditTrailDto.setPickupPointCode(Constants.HYPHEN);
    auditTrailDto.setName(product.getProductName());
    auditTrailDto.setBusinessPartnerCode(product.getMerchantCode());
    return auditTrailDto;
  }

  private List<AuditTrailDto> removeSizeChartCode(Map<String, Product> productsMap, Set<String> removeFromSkus, String sizeChartCode) {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    for (String removeFromSku : removeFromSkus) {
      Product product = productsMap.get(removeFromSku);
      String oldSizeChartCode = product.getSizeChartCode();
      AuditTrailDto auditTrailDto =
          getAuditTrailDto(Constants.HYPHEN, oldSizeChartCode, product,
          SIZE_CHART_REMOVAL_HISTORY);
      product.setSizeChartCode(StringUtils.EMPTY);
      auditTrailDtoList.add(auditTrailDto);
    }
    return auditTrailDtoList;
  }
}
