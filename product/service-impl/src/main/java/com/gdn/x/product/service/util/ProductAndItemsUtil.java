package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.BeanUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.WholesaleRule;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointSummaryRequestVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;

public class ProductAndItemsUtil {

  public static List<String> getItemCodeFromItem(Collection<Item> items) {
    return Optional.ofNullable(items).orElse(new HashSet<>()).stream()
        .filter(item -> StringUtils.isNotEmpty(item.getItemCode())).map(Item::getItemCode).distinct()
        .collect(Collectors.toList());
  }

  public static Set<String> getItemSkuFromItemPickupPoints(List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream().map(ItemPickupPoint::getItemSku)
        .collect(Collectors.toSet());
  }

  public static List<String> getProductSkuFromItemPickupPoints(List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
        .map(ItemPickupPoint::getProductSku).distinct().collect(Collectors.toList());
  }

  public static List<String> getPickupPointCodesFromItemPickupPoints(List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
        .map(ItemPickupPoint::getPickupPointCode).distinct().collect(Collectors.toList());
  }

  public static List<String> getCategoryFromProduct(Collection<Product> products, boolean usePcbData) {
    return products.stream().map(product -> getCategoryCode(product, usePcbData)).collect(Collectors.toList());
  }

  public static String getCategoryCode(Product product, boolean usePcbMasterData) {
    return usePcbMasterData || StringUtils.isBlank(product.getCategoryCode()) ?
        product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode() :
        product.getCategoryCode();
  }

  public static List<String> getCategoryFromProductSummaryResponseV2Vo(
      List<ProductSummaryResponseV2Vo> productSummaryResponseV2VoList) {
    return Optional.ofNullable(productSummaryResponseV2VoList).orElse(new ArrayList<>()).stream()
        .filter(response -> StringUtils.isNotEmpty(response.getCategoryCode()))
        .map(ProductSummaryResponseV2Vo::getCategoryCode).distinct().collect(Collectors.toList());
  }

  public static ItemPickupPointSummaryRequestVo toItemPickupPointSummaryRequestVo(
      ItemPickupPointSummaryRequest itemPickupPointSummaryRequest, Set<String> itemSkus) {
    ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo = new ItemPickupPointSummaryRequestVo();
    BeanUtils.copyProperties(itemPickupPointSummaryRequest, itemPickupPointSummaryRequestVo, "itemPickupPointCode");
    itemPickupPointSummaryRequestVo.setItemSkus(itemSkus);
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryRequest.getItemPickupPointCode())) {
      itemPickupPointSummaryRequestVo.setItemPickupPointCode(
          itemPickupPointSummaryRequest.getItemPickupPointCode().stream().map(
              itemPickupPointRequest -> new ItemPickupPointRequestVo(itemPickupPointRequest.getItemSku(),
                  itemPickupPointRequest.getPickupPointCode())).collect(Collectors.toList()));
    }
    return itemPickupPointSummaryRequestVo;
  }

  public static ProductSummaryRequestV2Vo toProductSummaryRequestV2Vo(ProductSummaryRequestV2 productSummaryRequestV2) {
    ProductSummaryRequestV2Vo productSummaryRequestV2Vo = new ProductSummaryRequestV2Vo();
    BeanUtils.copyProperties(productSummaryRequestV2, productSummaryRequestV2Vo);
    return productSummaryRequestV2Vo;
  }

  public static HalalDashboardFilterRequestVo toHalalDashboardFilterRequestVo(
      HalalProductsFilterRequest halalProductsFilterRequest) {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo = new HalalDashboardFilterRequestVo();
    BeanUtils.copyProperties(halalProductsFilterRequest, halalDashboardFilterRequestVo);
    halalDashboardFilterRequestVo.getCurationStatus().addAll(
        halalProductsFilterRequest.getCurationStatus().stream().map(status -> CurationStatus.valueOf(status).getValue())
            .collect(Collectors.toList()));
    return halalDashboardFilterRequestVo;
  }

  public static List<ItemPickupPointRequestVo> toItemPickupPointRequestVo(
      List<ItemPickupPointRequest> itemPickupPointRequestList) {
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoList = new ArrayList<>();
    for (ItemPickupPointRequest itemPickupPointRequest : itemPickupPointRequestList) {
      ItemPickupPointRequestVo itemPickupPointRequestVo = new ItemPickupPointRequestVo();
      BeanUtils.copyProperties(itemPickupPointRequest, itemPickupPointRequestVo);
      itemPickupPointRequestVoList.add(itemPickupPointRequestVo);
    }
    return itemPickupPointRequestVoList;
  }

  public static void validateItemPickupPointRequest(List<ItemPickupPointRequest> itemPickupPointRequestList) {
    for (ItemPickupPointRequest itemPickupPointRequest : itemPickupPointRequestList) {
      GdnPreconditions.checkArgument(
          StringUtils.isNotBlank(itemPickupPointRequest.getItemSku()) && StringUtils.isNotBlank(
              itemPickupPointRequest.getPickupPointCode()),
          ErrorMessages.ITEM_AKU_AND_PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
  }

  public static ProductItemsVo toProductItemsVo(Product product, Item item, ItemPickupPoint itemPickupPoint,
      boolean copyL5Fields, boolean validateViewConfig) {
    ProductVo productVo = new ProductVo();
    BeanUtils.copyProperties(product, productVo);

    ItemVo itemVo = new ItemVo();
    BeanUtils.copyProperties(item, itemVo);

    if (copyL5Fields) {
      copyItemPickupPointDetailsToItemVo(itemVo, itemPickupPoint, validateViewConfig);
    }

    return new ProductItemsVo(productVo, Arrays.asList(itemVo));
  }

  private static void copyItemPickupPointDetailsToItemVo(ItemVo itemVo,
      ItemPickupPoint itemPickupPoint, boolean validateViewConfig) {
    itemVo.setMerchantSku(itemPickupPoint.getMerchantSku());
    itemVo.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemVo.setCncActive(itemPickupPoint.isCncActive());
    itemVo.setPrice(itemPickupPoint.getPrice());
    if (validateViewConfig) {
      itemVo.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs());
    } else {
      itemVo.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
    }
    itemVo.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    itemVo.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemVo.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    itemVo.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    itemVo.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
  }

  public static List<WholesaleRuleVO> toWholesaleRuleVO(List<WholesaleRule> wholesaleRuleList) {
    List<WholesaleRuleVO> wholesaleRuleVOList = new ArrayList<>();
    for (WholesaleRule wholesaleRule : wholesaleRuleList) {
      WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
      BeanUtils.copyProperties(wholesaleRule, wholesaleRuleVO);
      wholesaleRuleVOList.add(wholesaleRuleVO);
    }
    return wholesaleRuleVOList;
  }

  public static ItemPickupPointListingRequestVo toItemPickupPointListingRequestVo(
    ItemPickupPointListingRequest itemPickupPointListingRequest, Set<String> itemSkus) {
    return ItemPickupPointListingRequestVo.builder()
      .businessPartnerCode(itemPickupPointListingRequest.getBusinessPartnerCode())
      .productSku(itemPickupPointListingRequest.getProductSku()).itemSkus(itemSkus)
      .pickupPointCodes(itemPickupPointListingRequest.getPickupPointCodes())
      .isFbbSortRequired(itemPickupPointListingRequest.isFbbSortRequired()).build();
  }

  public static Set<String> getItemSkuFromItem(List<Item> items) {
    return items.stream().map(Item::getItemSku).collect(Collectors.toSet());
  }

  public static List<Product> filterProductWithMasterDataDetails(Collection<Product> productList,
      boolean fetchPCBMasterData) {
    return !fetchPCBMasterData ?
        productList.stream().filter(product -> StringUtils.isNotEmpty(product.getCategoryCode()))
            .collect(Collectors.toList()) :
        new ArrayList<>();
  }

  public static List<Product> filterProductWithoutMasterDataDetails(Collection<Product> productList,
      boolean fetchPCBMasterData) {
    return !fetchPCBMasterData ?
        productList.stream().filter(product -> StringUtils.isEmpty(product.getCategoryCode()))
            .collect(Collectors.toList()) :
        new ArrayList<>(productList);
  }

  public static Set<String> getProductSkuFromProduct(Collection<Product> productList) {
    return productList.stream().map(Product::getProductSku).collect(Collectors.toSet());
  }

  public static List<Item> filterItemWithoutMasterDataDetails(List<Item> itemList, Set<String> productSkus) {
    return itemList.stream().filter(item -> productSkus.contains(item.getProductSku())).collect(Collectors.toList());
  }

  public static List<ProductAndItemsVO> filterProductAndItemsVOWithoutMasterDataDetails(
      List<ProductAndItemsVO> productAndItemsVOList, Set<String> productSkus) {
    return productAndItemsVOList.stream()
        .filter(productAndItemsVO -> productSkus.contains(productAndItemsVO.getProduct().getProductSku()))
        .collect(Collectors.toList());
  }

  public static Map<String, Item> getItemMapKeyItemSku(Collection<Item> items) {
    return items.stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldValue, newValue) -> newValue));
  }

  public static Map<String, ItemPickupPoint> getItemPickupPointMapKeyItemSku(
      Collection<ItemPickupPoint> itemPickupPoints) {
    return itemPickupPoints.stream()
        .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (oldValue, newValue) -> newValue));
  }

  public static List<String> getItemSkuFromItemListingUpdateRequestVo(
      List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos) {
    return itemListingUpdateRequestVos.stream().map(ItemListingUpdateRequestVo::getItemSku).distinct()
        .collect(Collectors.toList());
  }

  public static MasterCatalogDTO convertToMasterCatalogResponse(MasterCatalog masterCatalog) {
    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    if (Objects.nonNull(masterCatalog)) {
      BeanUtils.copyProperties(masterCatalog, masterCatalogDTO, "category");
      CategoryDTO categoryDTO = new CategoryDTO();
      if(Objects.nonNull(masterCatalog.getCategory())) {
        categoryDTO.setCategoryCode(masterCatalog.getCategory().getCategoryCode());
        categoryDTO.setCatgroupId(masterCatalog.getCategory().getCatgroupId());
      }
      masterCatalogDTO.setCategory(categoryDTO);
    }
    return masterCatalogDTO;
  }

  public static boolean isPreOrderActive(Date preOrderEndDate) {
    return Objects.nonNull(preOrderEndDate) && new Date().before(preOrderEndDate);
  }
}
