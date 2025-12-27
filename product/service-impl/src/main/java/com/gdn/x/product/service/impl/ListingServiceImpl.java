package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ListingService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ListingServiceImpl implements ListingService {

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ItemPriceService itemPriceService;

  @Value("${master.category.catalog.code}")
  private String masterCatalogCode;

  @Value("${validate.summary.request}")
  private boolean validateSummaryRequest;

  @Value("${replace.offer.price.with.merchant.promo.price.in.seller.side}")
  private boolean replaceOfferPriceWithMerchantPromoPriceInSellerSide;

  @Value("${replace.merchant.promo.discount.based.on.end.date}")
  private boolean replaceMerchantPromoDiscountBasedOnEndDate;

  @Value("${ignore.generating.merchant.promo.discount.when.promo.price.is.null}")
  private boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull;

  @Value("${fetch.item.by.product.sku.and.item.sku}")
  private boolean fetchItemByProductSkuAndItemSku;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${populate.null.for.wholesale.price.activated}")
  private boolean populateNullForWholesalePriceActivated;

  @Value("${use.pcb.master.data}")
  private boolean usePcbMasterData;

  @Override
  public Page<ItemResponseV2> getItemPickupPointSummary(String storeId, int page, int size,
      ItemPickupPointSummaryRequest itemPickupPointSummaryRequest, String fetchViewConfigByChannel) throws Exception {
    validateArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    validateArgument(Objects.nonNull(itemPickupPointSummaryRequest), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);

    Set<String> itemSkus = new HashSet<>();
    if (CommonUtil.isItemSkuRequired(itemPickupPointSummaryRequest.getItemCodes(),
        itemPickupPointSummaryRequest.getKeyword(), itemPickupPointSummaryRequest.getItemSku())) {
      itemSkus = getItemSkusUsingItemSkuOrItemCodeOrItemName(storeId, itemPickupPointSummaryRequest.getMerchantCode(),
          itemPickupPointSummaryRequest.getItemSku(), itemPickupPointSummaryRequest.getItemCodes(),
          itemPickupPointSummaryRequest.getKeyword());
      if (CollectionUtils.isEmpty(itemSkus)) {
        return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
      }
    }

    if (validateSummaryRequest && !CommonUtil.isValidItemPickupPointSummaryRequest(
      itemPickupPointSummaryRequest)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessages.INVALID_ITEM_SUMMARY_REQUEST);
    }

    Page<ItemPickupPoint> itemPickupPointPage =
        itemPickupPointService.findItemPickupPointForSummaryListing(storeId, page, size, itemPickupPointSummaryRequest, itemSkus);

    if (CollectionUtils.isEmpty(itemPickupPointPage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }

    Map<String, Item> itemMap = fetchItemByItemSkus(storeId, itemPickupPointPage.getContent(), StringUtils.EMPTY, false);
    Map<String, Product> productMap = fetchProductByProductSkus(storeId, itemPickupPointPage.getContent());
    Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap =
        fetchBusinessPartnerPickupPointByPickupPointCodes(storeId, itemPickupPointPage.getContent());
    setMasterData(storeId, productMap.values(), itemMap.values(), usePcbMasterData);
    Map<String, List<CategoryResponse>> categoryHierarchyMap = fetchCategoryHierarchyByCategoryCode(
        ProductAndItemsUtil.getCategoryFromProduct(productMap.values(), usePcbMasterData));

    return new PageImpl<>(ResponseHelper.toItemResponseV2(itemPickupPointPage.getContent(), itemMap, productMap,
            businessPartnerPickupPointMap, categoryHierarchyMap, usePcbMasterData,
            cncForWarehouseFeatureSwitch, fetchViewConfigByChannel),
        PageRequest.of(page, size), itemPickupPointPage.getTotalElements());
  }

  @Override
  public Page<ItemResponseV2> getItemPickupPointsByItemSku(String storeId, int page, int size,
      ItemRequestV2 itemRequestV2, boolean excludeDistributionPickupPoint) throws Exception {
    validateArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    validateArgument(Objects.nonNull(itemRequestV2), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    validateArgument(CollectionUtils.isNotEmpty(itemRequestV2.getItemSkuList()), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);

    Page<ItemPickupPoint> itemPickupPointPage =
        itemPickupPointService.findItemPickupPointByItemSkus(storeId, itemRequestV2, page, size, excludeDistributionPickupPoint);

    if (CollectionUtils.isEmpty(itemPickupPointPage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }

    Map<String, Item> itemMap = fetchItemByItemSkus(storeId, itemPickupPointPage.getContent(), StringUtils.EMPTY, false);
    Map<String, Product> productMap = fetchProductByProductSkus(storeId, itemPickupPointPage.getContent());
    Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap =
        fetchBusinessPartnerPickupPointByPickupPointCodes(storeId, itemPickupPointPage.getContent());
    setMasterData(storeId, productMap.values(), itemMap.values(), usePcbMasterData);
    Map<String, List<CategoryResponse>> categoryHierarchyMap =
        fetchCategoryHierarchyByCategoryCode(ProductAndItemsUtil.getCategoryFromProduct(productMap.values(), usePcbMasterData));

    return new PageImpl<>(ResponseHelper.toItemResponseV2(itemPickupPointPage.getContent(), itemMap, productMap,
            businessPartnerPickupPointMap, categoryHierarchyMap, usePcbMasterData,
            cncForWarehouseFeatureSwitch, Constants.ALL),
        PageRequest.of(page, size), itemPickupPointPage.getTotalElements());
  }

  @Override
  public Page<ProductSummaryResponseV2> getProductSummary(String storeId, int page, int size,
      ProductSummaryRequestV2 productSummaryRequestV2) {
    validateArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    validateArgument(Objects.nonNull(productSummaryRequestV2), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);

    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSearchService.getProductSummaryResponseV2(storeId, page, size, productSummaryRequestV2);

    if (CollectionUtils.isEmpty(productSummaryResponseV2VoPage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }

    Map<String, List<CategoryResponse>> categoryHierarchyMap = fetchCategoryHierarchyByCategoryCode(
        ProductAndItemsUtil.getCategoryFromProductSummaryResponseV2Vo(productSummaryResponseV2VoPage.getContent()));

    return new PageImpl<>(
        ResponseHelper.toProductSummaryResponseV2(productSummaryResponseV2VoPage.getContent(), categoryHierarchyMap),
        PageRequest.of(page, size), productSummaryResponseV2VoPage.getTotalElements());
  }

  @Override
  public Page<HalalDashboardProductsResponse> getHalalDashboardProductsResponses(String storeId, int page, int size,
      HalalProductsFilterRequest halalProductsFilterRequest) {
    validateArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    Page<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoPage =
        productSearchService.getHalalDashboardProducts(storeId, page, size, halalProductsFilterRequest);
    if (CollectionUtils.isEmpty(halalDashboardProductsResponseVoPage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
    return new PageImpl<>(
        ResponseHelper.toHalalDashboardProductResponseList(halalDashboardProductsResponseVoPage.getContent()),
        PageRequest.of(page, size), halalDashboardProductsResponseVoPage.getTotalElements());
  }

  @Override
  public Page<ItemPickupPointListingResponse> getItemPickupPointListing(String storeId, int page, int size,
      String fetchViewConfigByChannel, ItemPickupPointListingRequest itemPickupPointListingRequest) {
    validateArgument(Objects.nonNull(itemPickupPointListingRequest),
        ErrorMessages.ITEM_PICKUP_POINT_LISTING_REQUEST_CANNOT_BE_NULL);
    validateArgument(StringUtils.isNotBlank(itemPickupPointListingRequest.getBusinessPartnerCode()),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    validateArgument(StringUtils.isNotBlank(itemPickupPointListingRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_EMPTY);
    Set<String> filterItemSkus = getItemSkuByProductskuAndKeyword(storeId, itemPickupPointListingRequest);

    if (StringUtils.isNotBlank(itemPickupPointListingRequest.getKeyword()) && CollectionUtils.isEmpty(filterItemSkus)) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }

    Page<ItemPickupPoint> itemPickupPointPage =
        itemPickupPointService.getItemPickupPointListingByProductSku(storeId, page, size,
            ProductAndItemsUtil.toItemPickupPointListingRequestVo(itemPickupPointListingRequest, filterItemSkus));

    if (!itemPickupPointListingRequest.isResponseWithoutPickupPoint() && CollectionUtils.isEmpty(
        itemPickupPointPage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    } else if (itemPickupPointListingRequest.isResponseWithoutPickupPoint() && CollectionUtils.isEmpty(
        itemPickupPointPage.getContent())) {
      Item item = getItem(storeId, itemPickupPointListingRequest);
      Product product =
          productService.findByStoreIdAndProductSku(storeId, itemPickupPointListingRequest.getProductSku());
      if (Objects.isNull(product) || Objects.isNull(item)) {
        return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
      }
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList =
          ResponseHelper.toItemPickupPointListingResponseWithoutL5(product, item);
      return new PageImpl<>(itemPickupPointListingResponseList, PageRequest.of(page, size),
          itemPickupPointListingResponseList.size());
    }

    Map<String, Item> itemMap =
        fetchItemByItemSkus(storeId, itemPickupPointPage.getContent(), itemPickupPointListingRequest.getProductSku(),
            fetchItemByProductSkuAndItemSku);
    Map<String, Product> productMap = fetchProductByProductSkus(storeId, itemPickupPointPage.getContent());
    Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap =
        fetchBusinessPartnerPickupPointByPickupPointCodes(storeId, itemPickupPointPage.getContent());
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointPage.getContent();

    Map<String, Double> itemPickupPointAndOriginalPriceMap = itemPriceService
        .getOriginalSellingPrice(itemPickupPoints);
    if (replaceOfferPriceWithMerchantPromoPriceInSellerSide) {
      Map<String, Set<Price>> itemPickupPointPriceMap =
          itemPriceService.getDiscountItemPickupPoints(itemPickupPoints);
      for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
        if (itemPickupPointPriceMap.containsKey(
            itemPickupPoint.getItemSku() + Constants.DASH + itemPickupPoint.getPickupPointCode())) {
          itemPickupPoint.setPrice(itemPickupPointPriceMap.get(
              itemPickupPoint.getItemSku() + Constants.DASH + itemPickupPoint.getPickupPointCode()));
        }
      }
    }

    return new PageImpl<>(ResponseHelper.toItemPickupPointListingResponse(itemPickupPoints, productMap, itemMap,
        businessPartnerPickupPointMap, replaceOfferPriceWithMerchantPromoPriceInSellerSide,
        itemPickupPointAndOriginalPriceMap, replaceMerchantPromoDiscountBasedOnEndDate,
        ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull, fetchViewConfigByChannel,
        cncForWarehouseFeatureSwitch, populateNullForWholesalePriceActivated),
        PageRequest.of(page, size), itemPickupPointPage.getTotalElements());
  }

  private Item getItem(String storeId, ItemPickupPointListingRequest itemPickupPointListingRequest) {
    Item item = itemService.findByStoreIdAndItemSku(storeId, itemPickupPointListingRequest.getItemSku());
    if (fetchItemByProductSkuAndItemSku) {
      if (Objects.nonNull(item) && !StringUtils.equals(itemPickupPointListingRequest.getProductSku(),
          item.getProductSku())) {
        item = null;
      }
    }
    return item;
  }

  private Set<String> getItemSkusUsingItemSkuOrItemCodeOrItemName(String storeId, String merchantCode, String itemSku,
      Set<String> itemCodes, String keyword) {
    Set<String> itemSkus = new HashSet<>();

    if (CommonUtil.isRequestHasItemCodeOrKeywordFilter(itemCodes, keyword)) {
      itemSkus.addAll(
          itemService.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(storeId, merchantCode, itemCodes, keyword));
    }

    if (StringUtils.isNotBlank(itemSku)) {
      itemSkus.add(itemSku);
    }

    return itemSkus;
  }

  private Set<String> getItemSkuByProductskuAndKeyword(String storeId,
      ItemPickupPointListingRequest itemPickupPointListingRequest) {
    Set<String> itemSkus = new HashSet<>();
    if (StringUtils.isNotBlank(itemPickupPointListingRequest.getKeyword())) {
      itemSkus.addAll(itemService.getItemSkuByItemNameKeyword(storeId, itemPickupPointListingRequest.getProductSku(),
          itemPickupPointListingRequest.getKeyword()));
    }
    if (StringUtils.isNotBlank(itemPickupPointListingRequest.getItemSku())) {
      itemSkus.add(itemPickupPointListingRequest.getItemSku());
    }
    return itemSkus;
  }

  private Map<String, BusinessPartnerPickupPoint> fetchBusinessPartnerPickupPointByPickupPointCodes(String storeId,
      List<ItemPickupPoint> itemPickupPointList) {
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList = Optional.ofNullable(
            this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
                ProductAndItemsUtil.getPickupPointCodesFromItemPickupPoints(itemPickupPointList)))
        .orElse(new ArrayList<>());
    return businessPartnerPickupPointList.stream()
        .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, Function.identity()));
  }

  private Map<String, Product> fetchProductByProductSkus(String storeId, List<ItemPickupPoint> itemPickupPointList) {
    List<Product> productList = Optional.ofNullable(this.productService.findByStoreIdAndProductSkuIn(storeId,
        ProductAndItemsUtil.getProductSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    return productList.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
  }

  private Map<String, Item> fetchItemByItemSkus(String storeId, List<ItemPickupPoint> itemPickupPointList,
      String productSku, boolean fetchItemByProductSkuAndItemSku) {
    List<Item> itemList = Optional.ofNullable(this.itemService.getItemsByStoreIdAndItemSkus(storeId,
        ProductAndItemsUtil.getItemSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    if (fetchItemByProductSkuAndItemSku) {
      itemList = itemList.stream().filter(item -> StringUtils.equals(item.getProductSku(), productSku))
          .collect(Collectors.toList());
    }
    return itemList.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
  }

  private Map<String, List<CategoryResponse>> fetchCategoryHierarchyByCategoryCode(List<String> categoryCodes) {
    Map<String, List<CategoryResponse>> categoryHierarchyMap = new HashMap<>();
    List<List<CategoryResponse>> categoryHierarchyResponseList =
        productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, categoryCodes);
    for (List<CategoryResponse> categoryResponseList : categoryHierarchyResponseList) {
      CategoryResponse categoryResponse =
          categoryResponseList.stream().filter(c -> categoryCodes.contains(c.getCategoryCode())).findFirst()
              .orElse(new CategoryResponse());
      categoryHierarchyMap.put(categoryResponse.getCategoryCode(), categoryResponseList);
    }
    return categoryHierarchyMap;
  }

  private void setMasterData(String storeId, Collection<Product> products, Collection<Item> items,
      boolean usePcbMasterData) throws Exception {
    List<Product> productWithMasterData =
        ProductAndItemsUtil.filterProductWithMasterDataDetails(products, usePcbMasterData);
    List<Product> productWithoutMasterData =
        ProductAndItemsUtil.filterProductWithoutMasterDataDetails(products, usePcbMasterData);

    if (CollectionUtils.isNotEmpty(productWithoutMasterData)) {
      Map<String, List<Item>> itemListMap = items.stream().collect(Collectors.groupingBy(Item::getProductSku));
      for (Product product : products) {
        if (product.isSynchronized()) {
          masterDataConstructorService.constructProductAndItemWithMasterData(storeId, Constants.DEFAULT_USERNAME,
              Constants.DEFAULT_REQUEST_ID, product, itemListMap.get(product.getProductSku()));
        }
      }
    }

    if (CollectionUtils.isNotEmpty(productWithMasterData)) {
      products.stream().filter(Product::isSynchronized).forEach(product -> product.setMasterCatalog(
          new MasterCatalog(masterCatalogCode, new Category(product.getCategoryCode(), product.getCategoryCode()))));
    }
  }

  private void validateArgument(boolean expression, String errorMessage) {
    GdnPreconditions.checkArgument(expression, errorMessage);
  }

  @Override
  public Page<ItemL5ListingResponse> getItemL5Listing(String storeId, List<String> productSkus, List<String> l5IdList,
      Integer page, Integer pageSize, Boolean cncActivated, boolean fetchOnlyBundleVariants) {
    ItemAndItemPickupPointVo itemAndItemPickupPointVo = null;
    if (Objects.isNull(pageSize) && Objects.isNull(page) && Objects.isNull(cncActivated)) {
      itemAndItemPickupPointVo =
          itemService.getAllItemAndPickupPointDetailsWithoutPagination(storeId, productSkus, l5IdList);
    } else if (Objects.nonNull(pageSize) && Objects.nonNull(page)) {
      itemAndItemPickupPointVo = itemService.getItemAndPickupPointDetailsForCnc(storeId, productSkus, l5IdList,
          Boolean.TRUE.equals(cncActivated), page, pageSize);
    }

    if (Objects.isNull(itemAndItemPickupPointVo)) {
      return new PageImpl<>(new ArrayList<>());
    }
    Set<String> pickupPointCodes =
        itemAndItemPickupPointVo.getItemPickupPoints().stream().map(ItemPickupPoint::getPickupPointCode)
            .collect(Collectors.toSet());
    List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
            new ArrayList<>(pickupPointCodes));
    Map<String, String> pickupPointCodeAndNameMap = businessPartnerPickupPoints.stream()
        .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::getName));
    Map<String, Item> itemMap =
        itemAndItemPickupPointVo.getItem().stream().collect(Collectors.toMap(Item::getItemSku, item -> item));

    List<ItemL5ListingResponse> itemL5ListingResponses = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemAndItemPickupPointVo.getItemPickupPoints()) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Price price = itemPickupPoint.getPrice().stream().findFirst().orElse(new Price());
      ItemL5ListingResponse itemL5ListingResponse =
          ItemL5ListingResponse.builder().mainImageUrl(item.getMainImageUrl())
              .itemSku(itemPickupPoint.getItemSku()).ppCode(itemPickupPoint.getPickupPointCode())
              .ppCodeName(pickupPointCodeAndNameMap.get(itemPickupPoint.getPickupPointCode()))
              .itemName(item.getGeneratedItemName())
              .offerPrice(price.getOfferPrice())
              .cncActivated(itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.CNC).isBuyable())
              .productSku(itemPickupPoint.getProductSku()).build();
      if (fetchOnlyBundleVariants && CollectionUtils.isNotEmpty(item.getBundleRecipe())) {
        itemL5ListingResponses.add(itemL5ListingResponse);
      } else if (!fetchOnlyBundleVariants) {
        itemL5ListingResponses.add(itemL5ListingResponse);
      }
    }
    return new PageImpl<>(itemL5ListingResponses, PageRequest.of(Objects.nonNull(page) ? page : 0,
        itemL5ListingResponses.size() == 0 ? 1 : itemL5ListingResponses.size()),
        itemAndItemPickupPointVo.getTotalL5Count());
  }
}
