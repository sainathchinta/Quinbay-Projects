package com.gdn.x.productcategorybase.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.repository.ProductItemUomInfoRepository;
import com.gdn.x.productcategorybase.service.DistributionInfoService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class DistributionInfoServiceBean implements DistributionInfoService {

  private final ProductItemUomInfoRepository productItemUomInfoRepository;

  private final ObjectMapper objectMapper;

  private final MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${seller.api.client.id}")
  private String sellerApiClientId;

  @Override
  @Transactional
  public List<ProductItem> updateDistributionInfo(String storeId, String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest, Product product) throws Exception {
    validateDistributionInfoUpdateRequest(productCode, distributionInfoUpdateRequest);
    List<ProductItem> updatedProductItems = new ArrayList<>();
    List<ProductItemUomInfo> productItemUomInfos =
        product.getProductItems().stream().filter(Predicate.not(ProductItem::isMarkForDelete))
            .map(ProductItem::getProductItemUomInfo)
            .filter(Objects::nonNull).toList();
    Map<String, ProductItemUomInfo> skuCodeToProductItemUomInfoMap = productItemUomInfos.stream()
        .collect(Collectors.toMap(ProductItemUomInfo::getSkuCode,
            productItemUomInfo -> productItemUomInfo));
    Map<String, ProductItem> skuCodeToProductItemMap =
        product.getProductItems().stream()
            .collect(Collectors.toMap(ProductItem::getSkuCode, productItem -> productItem));

    List<ProductItemUomInfo> updatedProductItemUomInfo = new ArrayList<>();
    for (ProductItemUomInfoDTO productItemUomInfoDTO : distributionInfoUpdateRequest.getProductItemUomInfoDTOS()) {
      if (skuCodeToProductItemUomInfoMap.containsKey(productItemUomInfoDTO.getSkuCode())) {
        GdnPreconditions.checkArgument(skuCodeToProductItemMap.containsKey(productItemUomInfoDTO.getSkuCode()),
            ErrorMessage.ITEM_NOT_FOUND_FOR_SKU_CODE.getMessage());
        ProductItemUomInfo productItemUomInfo = skuCodeToProductItemUomInfoMap.get(productItemUomInfoDTO.getSkuCode());
        checkIfOmniChannelSkuIsUpdated(productItemUomInfoDTO, skuCodeToProductItemMap, updatedProductItems);
        productItemUomInfo.setUom(objectMapper.writeValueAsString(productItemUomInfoDTO.getDimensionAndUomDTOList()));
        productItemUomInfo.setExpiry(productItemUomInfoDTO.getDistributionItemInfoRequest().isExpiry());
        productItemUomInfo.setOrigin(
            Origin.valueOf(productItemUomInfoDTO.getDistributionItemInfoRequest().getOrigin()));

        updatedProductItemUomInfo.add(productItemUomInfoRepository.save(productItemUomInfo));
      } else {
        GdnPreconditions.checkArgument(skuCodeToProductItemMap.containsKey(productItemUomInfoDTO.getSkuCode()),
            ErrorMessage.ITEM_NOT_FOUND_FOR_SKU_CODE.getMessage());
        checkIfOmniChannelSkuIsUpdated(productItemUomInfoDTO, skuCodeToProductItemMap, updatedProductItems);
        ProductItemUomInfo productItemUomInfo =
            ConverterUtil.convertToProductItemUomInfo(productItemUomInfoDTO, productCode,
                distributionInfoUpdateRequest.getSellerCode(),
                skuCodeToProductItemMap.get(productItemUomInfoDTO.getSkuCode()), objectMapper);
        productItemUomInfo.setStoreId(storeId);
        updatedProductItemUomInfo.add(productItemUomInfoRepository.save(productItemUomInfo));
      }
    }
    updateProductItemInProduct(product, updatedProductItemUomInfo);
    return updatedProductItems;
  }

  private void checkIfOmniChannelSkuIsUpdated(ProductItemUomInfoDTO productItemUomInfoDTO,
      Map<String, ProductItem> skuCodeToProductItemMap, List<ProductItem> updatedProductItems) {
    if (Optional.ofNullable(mandatoryParameterHelper.getClientId()).orElse(StringUtils.EMPTY)
        .contains(sellerApiClientId)) {
      ProductItem productItem = skuCodeToProductItemMap.get(productItemUomInfoDTO.getSkuCode());
      if (!StringUtils.equals(productItem.getOmniChannelSku(),
          productItemUomInfoDTO.getDistributionItemInfoRequest().getOmniChannelSku())) {
        productItem.setOmniChannelSku(productItemUomInfoDTO.getDistributionItemInfoRequest().getOmniChannelSku());
        updatedProductItems.add(productItem);
      }
    }
  }

  public void updateProductItemInProduct(Product product, List<ProductItemUomInfo> updatedProductItemUomInfo) {
    if (CollectionUtils.isNotEmpty(updatedProductItemUomInfo)) {
      Map<String, ProductItemUomInfo> productItemUomInfoMap = updatedProductItemUomInfo.stream()
          .collect(Collectors.toMap(ProductItemUomInfo::getSkuCode, Function.identity(), (a, b) -> a));
      for (ProductItem productItem : product.getProductItems()) {
        if (productItemUomInfoMap.containsKey(productItem.getSkuCode())) {
          productItem.setProductItemUomInfo(productItemUomInfoMap.get(productItem.getSkuCode()));
        }
      }
    }
  }

  @Override
  public Page<DistributionInfoPerSkuResponse> getDistributionInfo(String storeId,
      String productCode, Pageable pageable, Product product, Map<String, ProductItem> skuCodeAndProductItemMap) {
    Page<ProductItemUomInfo> productItemUomInfoPage =
        productItemUomInfoRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            productCode, pageable);
    List<DistributionInfoPerSkuResponse> distributionInfoPerSkuResponseList =
        productItemUomInfoPage.stream().map(
            productItemUomInfo -> CommonUtil.mapToDistributionInfoPerSkuResponse(productItemUomInfo,
                product, skuCodeAndProductItemMap)).collect(Collectors.toList());
    return new PageImpl<>(distributionInfoPerSkuResponseList, pageable,
        productItemUomInfoPage.getTotalElements());
  }

  private void validateDistributionInfoUpdateRequest(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(distributionInfoUpdateRequest.getSellerCode()),
        ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(distributionInfoUpdateRequest.getProductItemUomInfoDTOS()),
        ErrorMessage.PRODUCT_ITEM_UOM_INFO_REQUESTS_MUST_NOT_BE_EMPTY.getMessage());
    distributionInfoUpdateRequest.getProductItemUomInfoDTOS()
        .forEach(this::validateProductItemUomInfoRequest);
  }

  private void validateProductItemUomInfoRequest(ProductItemUomInfoDTO productItemUomInfoDTO) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productItemUomInfoDTO.getSkuCode()),
        ErrorMessage.ITEM_CODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(productItemUomInfoDTO.getDimensionAndUomDTOList()),
        ErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(
        Objects.nonNull(productItemUomInfoDTO.getDistributionItemInfoRequest()),
        ErrorMessage.PRODUCT_ITEM_UOM_INFO_MUST_NOT_BE_EMPTY.getMessage());
  }
}