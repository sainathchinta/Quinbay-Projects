package com.gdn.x.product.service.util;

import java.util.List;
import java.util.Map;

import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import org.springframework.data.domain.Page;

import com.gdn.common.web.base.BaseRequest;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.PristineMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.rest.web.model.ItemDetailRestWeb;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SimpleProductListRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ComboDetailResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.ItemAndBundlingInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.PristineProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailV2Response;
import com.gdn.x.product.rest.web.model.response.SimpleProductListResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;

/**
 * @author WCSUser
 */
public interface ModelConverter {

  /**
   * @param itemDTO must not be null
   * @return itemRequestVO
   */
  Item convertItemDTOToItem(ItemDTO itemDTO);


  <B extends BaseResponse, L> List<B> convertListToResponse(Iterable<L> entities,
      Class<B> responseClass);

  /**
   * @param productDTO must not be null
   * @return product
   */
  Product convertProductDTOToProduct(ProductDTO productDTO);

  <B, L extends BaseRequest> List<B> convertRequestListToModel(List<L> requests,
      Class<B> responseClass);

  <B, L extends BaseRequest> B convertRequestToModel(L request, Class<B> responseClass);

  /**
   * @param addProductAndItemsResponseVo
   * @return
   */
  GdnRestSingleResponse<AddProductAndItemsResponse> convertToAddProductAndItemsResponse(
      String requestId, AddProductAndItemsResponseVo addProductAndItemsResponseVo);

  /**
   * @param handlingFeeRequestRestWebList must not be null
   * @return never null
   * @throws ReflectiveOperationException to controller class
   */
  List<HandlingFeeRequest> convertToHandlingFeeRequestList(
      List<HandlingFeeRequestRestWeb> handlingFeeRequestRestWebList)
      throws ReflectiveOperationException;

  /**
   * @param handlingFeeResponse must not be null
   * @return never null
   * @throws ReflectiveOperationException to controller class
   */
  HandlingFeeResponseRestWeb convertToHandlingFeeResponseRestWeb(
      HandlingFeeResponse handlingFeeResponse) throws ReflectiveOperationException;

  /**
   * @param itemRequest must not be null
   * @return never null
   */
  Item convertToItem(ItemRequest itemRequest);

  /**
   * @param input can be null or empty list
   * @param page must not be null
   * @param totalSize must not be null
   * @param requestId must not be blank
   * @return never null
   */
  GdnRestListResponse<ItemDetailRestWeb> convertToItemDetailRestWebGdnRestListResponse(
      List<ProductItemDetailVO> input, int page, long totalSize, String requestId);

  /**
   * @param priceRequest must not be null
   * @return never null
   */
  Price convertToItemPrice(PriceRequest priceRequest);

  /**
   * @param price must not be null
   * @return never null
   */
  PriceDTO convertToItemPriceResponse(Price price);

  /**
   * @param input can be empty list
   * @param requestId must not be blank
   * @return never null
   */
  GdnRestListResponse<SimpleItemResponse> convertToItemPriceRestWebGdnRestListResponse(
      List<ItemPriceVO> input, String requestId);

  /**
   * @param item must not be null
   * @return never null
   */
  ItemResponse convertToItemResponse(Item item);

  /**
   * @param discountPriceRequest must not be null
   * @return never null
   * @throws ReflectiveOperationException
   */
  DiscountPrice convertToItemSalePrice(DiscountPriceDTO discountPriceRequest)
      throws ReflectiveOperationException;

  /**
   * @param discountPrice must not be null
   * @return never null
   * @throws ReflectiveOperationException
   */
  DiscountPriceDTO convertToItemSalePriceResponse(DiscountPrice discountPrice)
      throws ReflectiveOperationException;

  /**
   * @param voObject
   * @return
   */
  GdnRestListResponse<ItemSummaryResponse> convertToItemSummaryListResponse(String requestId,
      int page, int size, ItemSummaryPageResponseVo voObject);

  /**
   * @param requestId
   * @param voObject
   * @return
   */
  GdnRestSingleResponse<ItemSummaryResponse> convertToItemSummarySingleResponse(String requestId,
      ItemSummaryPageResponseVo voObject);

  /**
   * @param productViewConfigRequest must not be null
   * @return product view config
   */
  ItemViewConfig convertToItemViewConfig(ItemViewConfigRequest productViewConfigRequest);

  /**
   * Convert to itemSku and itemview config map
   *
   * @param productViewConfigListRequest
   * @return
   */
  Map<String, ItemViewConfig> convertToItemViewConfigMap(
      List<ItemViewConfigAndItemSkuRequest> productViewConfigListRequest);

  /**
   * @param masterCatalogRequest must not be null
   * @return master catalog
   */
  MasterCatalog convertToMasterCatalog(MasterCatalogRequest masterCatalogRequest);

  /**
   * @param masterCatalog must not be null
   * @return master catalog response
   */
  MasterCatalogDTO convertToMasterCatalogResponse(MasterCatalog masterCatalog);

  MasterDataDetailWithProductAndItemsResponse convertToMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo);

  /**
   *
   * @param masterDataDetailWithProductAndItemResponseVo
   * @return
   */
  MasterDataDetailWithProductAndItemResponse toMasterDataDetailWithProductAndItemResponse(
      MasterDataDetailWithProductAndItemResponseVo masterDataDetailWithProductAndItemResponseVo);

  /**
   *
   * @param masterDataDetailResponseVo
   * @return
   */
  PristineMasterDataDetailResponse convertToPristineMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo);

  /**
   * Convert pristineMasterDataDetailResponseVo to pristineMasterDataDetailResponse, It contains
   * Product Items of PristineMaster as well
   * @param pristineMasterDataDetailResponseVo must not be null
   * @return PristineMasterDataDetailWithProductAndItemsResponse
   */
  PristineMasterDataDetailWithProductAndItemsResponse convertToPristineMasterDataDetailResponse(
      PristineMasterDataDetailWithProductAndItemsResponseVo pristineMasterDataDetailResponseVo);

  /**
   *
   * @param pristineProductAndItemsResponseVO
   * @return
   */
  PristineProductAndItemsResponse convertToPristineProductAndItemsResponse(
      PristineProductAndItemsResponseVO pristineProductAndItemsResponseVO);

  /**
   * @param productRequest must not be null
   * @return product
   */
  Product convertToProduct(ProductRequest productRequest);


  /**
   * @param productItemsVo must not be null
   * @return ProductAndItemInfoResponse
   */
  ProductAndItemInfoResponse convertToProductAndItemInfoResponse(
      ProductItemsVo productItemsVo);


  /**
   * @param productAndItemsVO must not be null
   * @param convertPreOrderDetails
   * @return never null
   */
  ProductAndItemsResponse convertToProductAndItemsDTO(ProductAndItemsVO productAndItemsVO, boolean convertPreOrderDetails);


  /**
   * @param productAndItemsVO must not be null
   * @return never null
   */
  ProductAndItemsResponse convertToProductAndItemsDTO(ProductAndItemsVO productAndItemsVO);

  /**
   * conver product to product response
   *
   * @param product
   * @return
   */
  ProductResponse convertProductToProductResponse(Product product);

  /**
   * @param productAndItemActivationRequest must not be null
   * @return
   */
  ProductItemsVo convertToProductAndItemsRequestVO(ProductAndItemActivationRequest productAndItemActivationRequest);

  /**
   * @param productVOs must not be null
   * @return never null
   */
  List<ProductForTransactionResponse> convertToProductForTransactionResponse(
      List<ProductForTransactionVO> productVOs);

  /**
   * @param product must not be null
   * @return product response
   */
  ProductResponse convertToProductResponse(Product product);


  /**
   * @param products
   * @return product response
   */
  List<ProductResponse> convertToProductResponseList(List<Product> products);

  GdnRestListResponse<ProductSummaryResponse> convertToProductSummaryResponse(
      Page<ProductAndItemSolr> productAndItems, String requestId, int page, int size);

  /**
   * @param itemViewConfig must not be null
   * @return product view config response
   */
  ItemViewConfigDTO convertToProductViewConfigResponse(ItemViewConfig itemViewConfig);

  <B extends BaseResponse, L> B convertToResponse(L entity, Class<B> responseClass);

  /**
   * @param salesCatalogRequest must not be null
   * @return sales catalog
   */
  SalesCatalog convertToSalesCatalog(SalesCatalogRequest salesCatalogRequest);

  /**
   * @param salesCatalog must not be null
   * @return sales catalog response
   */
  SalesCatalogDTO convertToSalesCatalogResponse(SalesCatalog salesCatalog);

  List<SimpleProductRequestVo> convertToSimpleProductRequestVo(
      SimpleProductListRequest simpleProductRequestList);

  SimpleProductResponse convertToSimpleProductResponse(Product product, Item item);


  SimpleProductListResponse convertToSimpleProductResponses(List<Product> products);

  /**
   * @param systemParameterRequest must not be null
   * @param systemParameter
   * @return never null
   * @throws ReflectiveOperationException to controller class
   */
  SystemParameter convertToSystemParameter(SystemParameterRequest systemParameterRequest,
      SystemParameter systemParameter) throws ReflectiveOperationException;

  /**
   * @param systemParameter must not be null
   * @return never null
   * @throws ReflectiveOperationException to controller class
   */
  SystemParameterResponse convertToSystemParameterResponse(SystemParameter systemParameter)
      throws ReflectiveOperationException;

  /**
   * @param updateItemSummaryRequest
   * @return
   */
  UpdateItemSummaryRequestVo convertToUpdateItemSummaryVo(
      UpdateItemSummaryRequest updateItemSummaryRequest);

  List<ItemListingUpdateRequestVo> convertToItemListingUpdateRequestVo(ItemListingUpdateRequest itemListingUpdateRequest);

  /**
   * @param masterDataDetailWithProductAndItemsResponseVo must not be null
   * @return SimpleProductsAndItemsResponse response contains limited fields about products and items
   */
  List<SimpleProductsAndItemsResponse> convertToSimpleProductsAndItemsDTO(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo);

  /**
   * @param masterDataDetailWithProductAndItemsResponseVo must not be null
   * @return SimpleProductMasterDataDetailResponse response contains limited fields about products
   */
  List<SimpleProductMasterDataDetailResponse> convertToSimpleProductMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo) throws Exception;

  /**
   * Convert pristineDataItemDto to pristineDataItem
   * @param pristineDataItemDto
   * @return PristineDataItem
   */
  PristineDataItem convertToPristineDataItem(PristineDataItemDto pristineDataItemDto)
      throws ReflectiveOperationException;

  /**
   * @param productDetailVos must not be null
   * @return ProductDetailResponse response contains products for official store api
   */
  List<ProductDetailResponse> convertToProductDetailResponse(List<ProductDetailVo> productDetailVos);

  /**
   * @param activeProductDetailVoList must not be null
   * @return ActiveProductResponse contains products for official store api
   */
  List<ActiveProductResponse> convertToActiveProductResponse(List<ActiveProductDetailVo> activeProductDetailVoList);

  /**
   * @param simplePristineProductRequestVoList must not be null
   * @return SimplePristineProductResponse response contains productCodes and productskus
   */
  List<SimplePristineProductResponse> convertToSimplePristineProductResponse(
      List<SimplePristineProductRequestVo> simplePristineProductRequestVoList) throws Exception;

  /**
   * @param productDetailRequest must not be null
   * @return OfficialStoreRequestVO reqeuest for create request vo to get official store products
   */
  OfficialStoreRequestVO toOfficialStoreRequestVO(
          ProductDetailRequest productDetailRequest);

  /**
   * @param offlineItemRequestList must not be null
   * @param merchantCode must not be blank
   * @param storeId must not be blank
   * @return List of OfflineItem
   */
  List<UpsertOfflineItemRequest> convertToOfflineItemList(List<OfflineItemRequest> offlineItemRequestList,
      String merchantCode, String storeId) throws Exception;

  /**
   * @param upsertOfflineItemRequests must not be null
   * @param merchantCode must not be blank
   * @param storeId must not be blank
   * @return List of OfflineItem
   */
  List<OfflineItem> convertToOfflineItems(List<UpsertOfflineItemRequest> upsertOfflineItemRequests,
      String merchantCode, String storeId)throws Exception;

  /**
   * @param merchantSkus must not be null
   * @param items must not be null
   * @return OfflineItemResponseDetail
   */
  List<OfflineItemResponseDetail> convertItemToOfflineProductResponse
      (List<String> merchantSkus, List<Item> items, Map<String, ProductType> productTypeByProductSkuMap);

  /**
   * @param productAttributeRequest must not be null
   * @return MasterDataProductAttribute
   */
  MasterDataProductAttribute convertProductAttributeRequestToMasterDataProductAttribute(
      ProductAttributeRequest productAttributeRequest) throws Exception;


  /**
   * @param productAndItemsVOs must not be null
   * @return never null
   */
  List<ProductAndItemsResponse> convertToProductAndItemsDTOs(List<ProductAndItemsVO> productAndItemsVOs);

  /**
   * @param comboDetailVo must not be null
   * @return comboDetailResponse
   */
  ComboDetailResponse convertComboDetailVoToComboDetailResponse(
          ComboDetailVo comboDetailVo);

  ActiveComboRequestVO convertActiveComboRequestToActiveComboRequestVO(
      ActiveComboRequest activeComboRequest);

  /**
   * @param wholesaleVO must not be null
   * @return wholesaleResponse
   */
  WholesaleResponse convertWholesaleVOToWholesaleResponse(WholesaleVO wholesaleVO);

  /**
   * @param comboResponseVO must not be null
   * @return ComboResponse
   */
  ComboResponse convertComboResponseVOToComboResponse(ComboResponseVO comboResponseVO);

  ItemAndBundlingInfoResponse convertPromoItemVOToPromoItemResponse(ItemAndBundlingInfoVO itemAndBundlingInfoVO);

  /**
   * method to convert itemPrice Vo list to itemPriceList Response
   *
   * @param itemPriceVOS itemPrice VO list
   * @return return itemPrice list response
   */
  List<ItemPriceResponse> convertItemPriceVoToItemPriceListResponse(List<ItemPriceVO> itemPriceVOS);

  /**
   * To Convert simpleMasterDataDetailWithProductAndItemsResponse to response
   * @param simpleMasterDataDetailWithProductAndItemsResponse
   * @return
   */
  SimpleProductAndItemsMasterDataDetailResponse toSimpleProductAndItemsMasterDataDetailResponse(
      SimpleMasterDataDetailWithProductAndItemsResponseVo
          simpleMasterDataDetailWithProductAndItemsResponse);

  /**
   * To Convert SimpleProductAndItemsMasterDataDetailV2Response to response
   * @param simpleMasterDataDetailWithProductAndItemsResponse
   * @return
   */
  SimpleProductAndItemsMasterDataDetailV2Response toSimpleProductAndItemsMasterDataDetailV2Response(
      SimpleMasterDataDetailWithProductAndItemsV2ResponseVo simpleMasterDataDetailWithProductAndItemsResponse);

  /**
   * @param activeProductRequest must not be null
   * @return ActiveProductsRequestVO request vo to get official store products
   */
  ActiveProductsRequestVO toActiveProductsRequestVO(ActiveProductRequest activeProductRequest);

  /**
   *
   * @param offlineItemDetailVos
   * @return
   */
  List<OfflineItemDetailResponse> toOfflineItemDetailResponse(List<OfflineItemDetailVo> offlineItemDetailVos);


  /**
   * @param itemInfoVOS
   * @return
   */
  List<ItemSummaryResponse> convertToItemSummaryResponse(List<ItemInfoVO> itemInfoVOS);

  /**
   * @param merchantCode
   * @param storeId
   * @param request (extends BaseRequest)
   * @return
   */
  <T extends BaseRequest> OfflineItem constructOfflineItem(String merchantCode, String storeId,
      T request);


  /**
   * Set promo types in ItemSummaryPageResponseVo
   *
   * @param ItemSummaryPageResponseVo
   * @return
   */
  void setPromoTypesForListOfItems(ItemSummaryPageResponseVo ItemSummaryPageResponseVo);

  /**
   * @param requestId
   * @param page
   * @param size
   * @param result
   * @param fetchViewConfigByChannel
   *
   * @return
   */
  GdnRestListResponse<ItemSummaryDetailResponse> convertToItemSummaryDetailListResponse(String requestId, int page, int size, ItemSummaryPageResponseVo result,
      String fetchViewConfigByChannel);

  /**
   *
   * @param productAndItemsVO
   * @return
   */
  ProductL3Response convertToProductL3Response(ProductAndItemsVO productAndItemsVO);

  /**
   * get item pickup point response
   * @param itemPickupPointVos
   * @return
   */
  List<ItemPickupPointCodeResponse> getItemPickupPointCodeResponseFromItemPickupPointVo(List<ItemPickupPointVo> itemPickupPointVos);

  /**
   * convert to ItemPickupPoint from ItemDto
   * @param itemVo
   * @param itemPickupPointActivationRequests
   * @param productSku
   * @param online
   * @return
   */
  List<ItemPickupPointVo> toItemPickupPointFromItemVo(Item itemVo, List<ItemPickupPointActivationRequest> itemPickupPointActivationRequests,
      String productSku, boolean online);

  /**
   * Convert to ProductAndItemsResponse from ProductItemsVo
   *
   * @param productItemsVo
   * @return
   */
  ProductAndItemsResponse convertToProductAndItemsResponse(ProductItemsVo productItemsVo, String fetchViewConfigByChannel, boolean validateViewConfig);

  /**
   *
   * @param productItemsVo
   * @param convertPreOrderDetails
   * @return
   */
  ProductAndItemsResponse convertToProductAndItemsResponseWithConvertPreOrderDetails(
      ProductItemsVo productItemsVo, boolean convertPreOrderDetails,
      String fetchViewConfigByChannel, boolean validateViewConfig);

  /**
   * Convert to productAndItemDataResponse
   * @param productItemsVo
   * @return
   */
  ProductAndItemDataResponse convertToProductAndItemDataResponse(ProductItemsVo productItemsVo);

  /**
   * To MasterDataDetailWithProductAndItemsResponse from MasterDataWithProductItemsVo
   *
   * @param masterDataWithProductItemsVo
   * @return
   */
  MasterDataDetailWithProductAndItemsResponse convertToMasterDataWithProductItemsVo(
    MasterDataWithProductItemsVo masterDataWithProductItemsVo);

  /**
   *To ItemPickupPointListingUpdateRequestVo from ItemPickupPointListingUpdateRequest
   *
   * @param itemPickupPointListingUpdateRequest
   * @return
   */
  List<ItemPickupPointListingUpdateRequestVo> convertToItemPickupPointListingUpdateRequestVo(
    ItemPickupPointListingUpdateRequest itemPickupPointListingUpdateRequest);

  /**
   * To ItemPickupPointUpdateRequestVo from ItemPickupPointUpdateRequest
   *
   * @param itemPickupPointUpdateRequest
   * @return
   */
  ItemPickupPointUpdateRequestVo covertToItemPickupPointUpdateRequestVo(
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest);

  /**
   * @param productItemsVo must not be null
   * @return ProductAndItemInfoResponse
   */
  ProductAndItemInfoResponseV2 convertToProductAndItemInfoResponseV2(
      ProductItemsVo productItemsVo);

  /**
   * Convert to pickup point detail
   *
   * @param businessPartnerPickupPoints
   * @return
   */
  List<PickupPointDetailResponse> convertToPickupPointDetailResponseList(
    List<BusinessPartnerPickupPoint> businessPartnerPickupPoints, boolean isFbb);

  /**
   * convert to basic product and item response
   * @param basicProductAndItemDTO
   * @param fetchViewConfigByChannel
   * @return
   */
  BasicProductAndItemResponse toBasicProductAndItemResponse(BasicProductAndItemDTO basicProductAndItemDTO, String fetchViewConfigByChannel) throws Exception;

  /**
   * @param product
   * @param attributeCodeValueAndValueType
   */
  void populateAttributeCodeValueAndValueType(Product product,
      Map<String, Map<String, String>> attributeCodeValueAndValueType);

  /**
   * Update sizeChartResponse
   *
   * @param sizeChartResponse
   * @return
   */
  SizeChartResponse updateSizeChartResponse(SizeChartResponse sizeChartResponse);
}
