package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.vo.MasterDataCacheVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.PCBMasterDataService;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PCBMasterDataServiceImpl implements PCBMasterDataService {

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Value("${always.fetch.in.all.product.data.from.pcb}")
  private boolean alwaysFetchInAllProductDataFromPCB;

  @Override
  @Cacheable(cacheManager = Constants.MASTER_DATA_CACHE_MANAGER, value = {
      CacheNames.GET_PRODUCT_DATA_WITH_BRAND_LOGO}, key = "#productCode", unless = "#result == null")
  public MasterDataCacheVo getProductDetailByProductCodeForAllProductsCached(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception {
    return getProductDetailByProductCodeForAllProductsWithoutCache(requestId, username, productCode, inAllProducts);
  }

  @Override
  public MasterDataCacheVo getProductDetailByProductCodeForAllProductsWithoutCache(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception {
    String brandCode = StringUtils.EMPTY;
    String brandLogoUrl = StringUtils.EMPTY;
    ProductDetailResponse productResponse = null;
    if (alwaysFetchInAllProductDataFromPCB) {
      productResponse =
          this.productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(requestId, username, productCode,
              true);
    } else {
      productResponse =
          this.productCategoryBaseOutbound.getProductDetailByProductCodeForAllProducts(requestId, username, productCode,
              inAllProducts);
    }
    checkArgument(Objects.nonNull(productResponse), ErrorMessages.PRODUCT_NOT_FOUND_WITH_PRODUCT_CODE + productCode);
    List<ProductAttributeResponse> productAttributeResponse = productResponse.getProductAttributeResponses();
    if (CollectionUtils.isNotEmpty(productAttributeResponse)) {
      brandCode = getBrandCode(brandCode, productAttributeResponse);
    }
    if (StringUtils.isNotEmpty(brandCode)) {
      brandLogoUrl = productCategoryBaseOutbound.getBrandLogoUrl(brandCode);
    }
    return new MasterDataCacheVo(productResponse, brandLogoUrl);
  }

  private String getBrandCode(String brandCode, List<ProductAttributeResponse> productAttributeResponse) {
    for (ProductAttributeResponse productAttribute : productAttributeResponse) {
      String attributeName = productAttribute.getAttribute().getName();
      if (Constants.BRAND.equalsIgnoreCase(attributeName) && !productAttribute.isMarkForDelete()) {
        List<ProductAttributeValueResponse> productAttributeValueResponseList =
            productAttribute.getProductAttributeValues();
        if (productAttributeValueResponseList.size() == Constants.SINGLE_ROW && Objects.nonNull(
            productAttributeValueResponseList.stream().findFirst().get().getPredefinedAllowedAttributeValue())) {
          brandCode = productAttributeValueResponseList.get(0).getPredefinedAllowedAttributeValue()
              .getPredefinedAllowedAttributeCode();
          break;
        }
      }
    }
    return brandCode;
  }

}
