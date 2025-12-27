package com.gdn.mta.product.repository;

import static com.gdn.partners.pbp.commons.constants.Constants.DELIMITER;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityRequest;
import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityResponse;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;

@Repository
public class ProductLevel3RepositoryBean implements ProductLevel3Repository {

  @Autowired
  private XProductFeign xProductFeign;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3RepositoryBean.class);

  @Override
  public Map<String, String> getItemNameByItemSku(List<String> itemSkus) throws Exception {
    GdnRestSingleResponse<SimpleMapStringResponse> response =
        xProductFeign.getItemNameByItemSkus(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), false,
            new SimpleListStringRequest(new ArrayList<>(itemSkus)));
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public Map<String, String> getProductNameByProductSku(List<String> productSkus) throws Exception {
    GdnRestSingleResponse<SimpleMapStringResponse> response =
        xProductFeign.getProductNameByProductSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            new SimpleListStringRequest(new ArrayList<>(productSkus)));
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public ProductAndItemsResponse findDetailByGdnSku(String gdnSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductAndSingleItemByItemSku(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), gdnSku,
            false, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    ProductAndItemsResponse product = response.getValue();
    return product;
  }

  @Override
  public ProductAndItemsResponse findProductAndItemDetailByGdnSku(String gdnSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductDetailAndSingleItemByItemSku(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),getRequestId(), getUsername(), gdnSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    ProductAndItemsResponse product = response.getValue();
    return product;
  }

  @Override
  public ProductAndItemsResponse findDetailByProductSku(String productSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), false,
            productSku, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    ProductAndItemsResponse product = response.getValue();
    return product;
  }

  @Override
  public ProductAndItemsResponse findDetailByProductSkuForSuspension(String productSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), true,
            productSku, false);    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    ProductAndItemsResponse product = response.getValue();
    return product;
  }

  @Override
  public Long getProductCountByBrand(String brand) throws Exception {
    GdnRestSingleResponse<SimpleLongResponse> response =
        xProductFeign.getProductsCountByBrand(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),getRequestId(), getUsername(), brand);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION.UNSPECIFIED, "["
          + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    return response.getValue().getValue();
  }

  @Override
  public Page<ItemSummaryResponse> findSummaryByFilter(String businessPartnerCode, String itemName,
      List<String> gdnSkus, String categoryCode, Double salePrice, String pickupPointCode,
      Boolean display, Boolean buyable, Pageable pageRequest, Boolean isArchived, SortOrder sort)
      throws Exception {
    ItemSummaryRequest request = new ItemSummaryRequest();
    sort = sort == null ? new SortOrder() : sort;
    request.setMerchantCode(businessPartnerCode);
    request.setProductItemName(itemName);
    request.setItemSkus(gdnSkus);
    request.setMasterCategoryCode(categoryCode);
    request.setOfferPrice(salePrice);
    request.setPickupPointCode(pickupPointCode);
    request.setDiscoverable(display);
    request.setBuyable(buyable);
    request.setArchived(isArchived);
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeign.getListOfItemSummaryByFilter(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), sort.getOrderBy(),
            sort.getSortBy(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    Page<ItemSummaryResponse> itemSummaryResponses =
        new PageImpl<ItemSummaryResponse>(response.getContent(), pageRequest, response
            .getPageMetaData().getTotalRecords());
    return itemSummaryResponses;
  }

  public Page<ItemSummaryResponse> findSummaryByCategoryAndBrandFilter(
      CampaignItemSummaryRequest campaignItemSummaryRequest, PageRequest pageRequest, SortOrder sort)
      throws Exception {
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeign.getListOfItemSummaryByCategoryAndBrandFilter(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), sort.getOrderBy(),
            sort.getSortBy(), campaignItemSummaryRequest);
    Page<ItemSummaryResponse> itemSummaryResponses =
        new PageImpl<ItemSummaryResponse>(response.getContent(), pageRequest, response
            .getPageMetaData().getTotalRecords());
    return itemSummaryResponses;
  }

  @Override
  public Page<ItemSummaryResponse> findSummaryByFilter(ItemSummaryRequest filter,
      Pageable pageRequest, SortOrder sort) throws Exception {
    String orderBy = null;
    String sortBy = null;
    if (sort != null) {
      orderBy = sort.getOrderBy();
      sortBy = sort.getSortBy();
    }
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeign.getListOfItemSummaryByFilter(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            pageRequest.getPageNumber(), pageRequest.getPageSize(), sort.getOrderBy(),
            sort.getSortBy(), filter);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return new PageImpl<>(response.getContent(), pageRequest, response.getPageMetaData()
        .getTotalRecords());
  }

  @Override
  public void updateItemPrice(PriceRequest priceRequest, String itemSku) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateItemPrice(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku, priceRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void updateItemViewConfig(ItemViewConfigRequest itemViewConfigRequest, String itemSku)
      throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.updateItemViewConfig(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku, itemViewConfigRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public ProductAndItemsResponse synchronizeProduct(String productSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.synchronizeProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            productSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ProductAndItemsResponse unsynchronizeProduct(String productSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.unsynchronizeProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku, true);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ItemSummaryResponse updateSummary(String businessPartnerCode, String gdnSku,
      UpdateItemSummaryRequest request) throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> response =
        xProductFeign.updateItemSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), gdnSku,
            businessPartnerCode, request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    ItemSummaryResponse itemSummaryResponse = response.getValue();
    return itemSummaryResponse;
  }

  @Override
  public ItemSummaryResponse findSummaryByGdnSku(String gdnSku) throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> response =
    xProductFeign.getItemSummaryByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),getRequestId(), getUsername(), gdnSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    ItemSummaryResponse itemSummaryResponse = response.getValue();
    return itemSummaryResponse;
  }

  @Override
  public ItemSummaryResponse findSummaryByArchivedGdnSku(String gdnSku) throws Exception {
    GdnRestSingleResponse<ItemSummaryResponse> response =
        xProductFeign.getArchivedItemSummaryByItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), gdnSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    ItemSummaryResponse itemSummaryResponse = response.getValue();
    return itemSummaryResponse;
  }

  @Override
  public SimpleBooleanResponse checkPickupPointCodeUsed(String pickupPointCode) throws Exception {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        xProductFeign.isPickupPointCodeUsed(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            pickupPointCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    SimpleBooleanResponse simpleBooleanResponse = response.getValue();
    return simpleBooleanResponse;
  }

  @Override
  public void updateItemOff2OnActivate(String itemSku) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.activateOff2OnChannelActive(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public ItemResponse getItem(String itemSku) throws Exception {
    GdnRestSingleResponse<ItemResponse> response =
        xProductFeign.getItem(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), itemSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, new StringBuilder(
          response.getErrorCode()).append(response.getErrorMessage()).toString());
    }
    ItemResponse itemResponse = response.getValue();
    return itemResponse;
  }

  private String getUsername() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? GdnBaseLookup.DEFAULT_USERNAME
        : GdnMandatoryRequestParameterUtil.getUsername();
  }

  private String getRequestId() {
    return StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID()
        .toString() : GdnMandatoryRequestParameterUtil.getRequestId();
  }

  @Override
  public ProductAndItemsResponse getProduct(String productSku) throws Exception {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeign.getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(), false,
            productSku, false);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, new StringBuilder(
          response.getErrorCode()).append(response.getErrorMessage()).toString());
    }
    ProductAndItemsResponse productAndItemsResponse = response.getValue();
    return productAndItemsResponse;
  }

  @Override
  public void updateItemOff2OnDeactivate(String itemSku) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.deactivateOff2OnChannelActive(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void toggleArchiveItem(String itemSku, boolean doArchive) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.toggleArchiveItem(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), itemSku, doArchive);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void toggleSuspensionProduct(String productSku, boolean doSuspension) throws Exception {
    GdnBaseRestResponse response =
        xProductFeign.toggleSuspensionProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productSku, doSuspension);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void updateResignBusinessPartnerItems(String businessPartnerCode) throws Exception {
    GdnBaseRestResponse response = xProductFeign.updateResignMerchantItemsByMerchantCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
        businessPartnerCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public List<ItemPriceResponse> getItemSkusToFetchEstimatedPrice(String itemCode) throws Exception {
    GdnRestListResponse<ItemPriceResponse> response =
        xProductFeign.getItemSkusByItemCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            itemCode);
    if (!response.isSuccess()) {
      LOGGER.error("Error while getting Item SKUs for item {}", itemCode, response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public Page<ActiveProductResponse> getAllProducts(SummaryFilterRequest summaryFilterRequest, Pageable page,
      List<String> categoryCodes, String catalogCode, boolean isCatalogCodeRequired) throws Exception {
    GdnRestListResponse<ActiveProductResponse> response =
        xProductFeign.getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            page.getPageNumber(), page.getPageSize(),
            convertToActiveProductRequest(summaryFilterRequest, categoryCodes, catalogCode,
                isCatalogCodeRequired));
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<ActiveProductResponse>(response.getContent(), page, response.getPageMetaData().getTotalRecords());
    return activeProductResponses;
  }

  @Override
  public Page<ItemSummaryResponse> getSuspendedItemList(SummaryFilterRequest summaryFilterRequest, Pageable page,
      List<String> categoryCodes, String catalogCode, boolean isCatalogCodeRequired) throws Exception{
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeign.getSuspendedItemList(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            page.getPageNumber(), page.getPageSize(),
            convertToActiveProductRequest(summaryFilterRequest, categoryCodes, catalogCode,
                isCatalogCodeRequired));
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return new PageImpl<ItemSummaryResponse>(response.getContent(), page, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<ProductResponse> getProductsByProductCodeAndMerchantCode(String productCode, String merchantCode)
      throws Exception {
    GdnRestListResponse<ProductResponse> response =
        xProductFeign.getProductsByProductCodeAndMerchantCode(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            productCode, merchantCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  public Map<String, Boolean> campaignEligibilityForProductSku(
      PromoEligibilityRequest promoEligibilityRequest) throws ApplicationRuntimeException {
    GdnRestSingleResponse<PromoEligibilityResponse> promoEligibilityResponse =
        xProductFeign.getCampaignEligibilityForProductSkus(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), getRequestId(), getUsername(),
            promoEligibilityRequest);
    if (!promoEligibilityResponse.isSuccess() || Objects.isNull(
        promoEligibilityResponse.getValue())) {
      LOGGER.error("Error from x-product to get campaign eligibility for request {} ",
          promoEligibilityResponse, promoEligibilityResponse.getErrorMessage());
      return new HashMap<>();
    }
    return promoEligibilityResponse.getValue().getPromoEligibility();
  }

  private ActiveProductRequest convertToActiveProductRequest(SummaryFilterRequest summaryFilterRequest,
      List<String> categoryCodes, String catalogCode, boolean isCatalogCodeRequired) {
    ActiveProductRequest activeProductRequest = new ActiveProductRequest();
    activeProductRequest.setMerchantCode(summaryFilterRequest.getBusinessPartnerCode());
    activeProductRequest.setStatus(summaryFilterRequest.getSuspensionStatus());
    activeProductRequest.setSearchKey(summaryFilterRequest.getSearchKeyword());
    activeProductRequest.setNameKey(summaryFilterRequest.getNameKey());
    activeProductRequest.setSortType(summaryFilterRequest.getSortOrder());
    List<String> categories = new ArrayList<>();
    if (Objects.nonNull(categoryCodes)) {
      if (isCatalogCodeRequired) {
        for (String categoryCode : categoryCodes) {
          categories.add(catalogCode + DELIMITER + categoryCode);
        }
        activeProductRequest.setCategoryCodes(categories);
      } else {
        activeProductRequest.setCategoryCodes(categoryCodes);
      }
    }
    activeProductRequest.setPickupPointCodes(summaryFilterRequest.getPickupPointCodes());
    LOGGER
        .info("Active product request for retrieving the products and items for suspension : {}", activeProductRequest);
    return activeProductRequest;
  }
}
