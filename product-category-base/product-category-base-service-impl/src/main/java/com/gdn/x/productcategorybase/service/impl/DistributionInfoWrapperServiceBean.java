package com.gdn.x.productcategorybase.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.service.DistributionInfoService;
import com.gdn.x.productcategorybase.service.DistributionInfoWrapperService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class DistributionInfoWrapperServiceBean implements DistributionInfoWrapperService {

  private final DistributionInfoService distributionInfoService;

  private final ProductService productService;

  private final ProductItemServiceBean productItemServiceBean;

  private final DomainEventPublisherService domainEventPublisherService;

  private final ObjectMapper objectMapper;

  @Override
  public void updateDistributionInfoAndPublishProduct(String storeId, String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) throws Exception {
    Product product =
        productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
            productCode);
    productService.setCompleteProductDetailsCached(storeId, product, true);

    List<ProductItem> productItems =
        distributionInfoService.updateDistributionInfo(storeId, productCode, distributionInfoUpdateRequest, product);

    if (CollectionUtils.isNotEmpty(productItems)) {
      productItems.forEach(productItemServiceBean::saveProductItem);
    }

    if (Objects.nonNull(distributionInfoUpdateRequest.getDistributionInfoRequest())) {
      product.setDistributionInfo(objectMapper.writeValueAsString(
          distributionInfoUpdateRequest.getDistributionInfoRequest()));
      Product clonedProduct = new Product();
      BeanUtils.copyProperties(product, clonedProduct, "productCategories", "productAttributes",
          "productImages", "productItems");
      productService.saveAndFlush(clonedProduct);
    }

    if (CollectionUtils.isNotEmpty(productItems) || Objects.nonNull(
        distributionInfoUpdateRequest.getDistributionInfoRequest())) {
      productService.evictAllProductDetailCache(product.getStoreId(), product);
    }

    domainEventPublisherService.publishProductChangeCategory(product, null, false, false, false,
        false, new HashSet<>());
  }

  @Override
  public Page<DistributionInfoPerSkuResponse> getDistributionInfo(String storeId,
      String productCode, Pageable pageable) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_EMPTY.getMessage());
    Product product = productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
      GdnPreconditions.checkArgument(Objects.nonNull(product),
          ErrorMessage.PRODUCT_NOT_FOUND.getMessage());
      List<ProductItem> productItemsByStoreIdAndProductIdCached =
          productItemServiceBean.getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
    Map<String, ProductItem> skuCodeAndProductItemMap = productItemsByStoreIdAndProductIdCached.stream()
        .collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity(), (a, b) -> a));
    return distributionInfoService.getDistributionInfo(storeId, productCode, pageable, product,
        skuCodeAndProductItemMap);
  }
}