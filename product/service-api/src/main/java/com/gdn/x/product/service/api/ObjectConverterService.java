package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import org.springframework.data.domain.Page;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.PickupPointChange;
import com.gdn.x.businesspartner.domain.event.model.PickupPointVO;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboRuleVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.model.vo.PristineItemVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingVO;
import com.gdn.x.product.model.vo.SimpleMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;

public interface ObjectConverterService {

  /**
   * @param products
   * @param items
   * @param mapOfMasterDataItems
   * @return
   */
  List<ProductAndItemsVO> convertAndValidateMasterDataExists(List<Product> products,
      List<Item> items, Map<String, MasterDataProduct> mapOfMasterDataProducts,
      Map<String, MasterDataItem> mapOfMasterDataItems);

  /**
   * Get SimpleProduct and Simple Items from MasterDataProduct and masterDataItems
   * @param products
   * @param items
   * @param mapOfMasterDataProducts
   * @param mapOfMasterDataItems
   * @return
   */
  List<SimpleProductAndItemsVO> convertAndValidateSimpleMasterDataExists(List<Product> products,
      List<Item> items, Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts,
      Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems);

  /**
   * Get SimpleProduct, SimpleItems and SimpleItemPickupPoint from MasterDataProduct and masterDataItems
   * @param products
   * @param items
   * @param itemPickupPoints
   * @param mapOfMasterDataProducts
   * @param mapOfMasterDataItems
   * @return
   */
  List<SimpleProductAndItemsAndItemPickupPointV0> convertAndValidateSimpleMasterDataExistsV2(
      List<Product> products, List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts, Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems);

  /**
   * Create ProductAndItems response from master Data for pristine products.
   * @param products
   * @param items
   * @param mapOfMasterDataProducts
   * @param mapOfMasterDataItems
   * @return
   */
  List<ProductAndItemsVO> convertAndValidateMasterDataExistsForPristine(List<Product> products,
      List<Item> items, Map<String, MasterDataProduct> mapOfMasterDataProducts,
      Map<String, MasterDataItem> mapOfMasterDataItems);


  /**
   *
   * @param item
   * @return
   */
  PristineItemVO convertToPristineItem(Item item);

  /**
   * convert byte array to string
   * @param bytes
   * @return
   */
  String convertBytesToString(byte[] bytes);

  /**
   * @param product
   * @param items
   * @return
   */
  AddProductAndItemsResponseVo convertToAddProductAndItemsResponseVo(Product product,
      List<Item> items);

  /**
   *
   * @param product
   * @param items
   * @return
   */
  AddProductAndItemsResponseVo convertToAddProductItemsResponseVo(Product product, List<ItemVo> items);

  ItemSummaryPageResponseVo convertToItemSummary(ProductAndItemsVO productAndItems);

  /**
   * @param listOfCategoriesList
   * @return
   */
  List<ItemCatalogVO> convertToListOfItemCatalog(List<List<CategoryResponse>> listOfCategoriesList);

  /**
   * Get list of item catalog. ItemCatalogVOV2 supports addition of new instance variables with downstream compatibility.
   * @param listOfCategoriesList
   * @return
   */
  List<ItemCatalogVOV2> convertToListOfItemCatalogV2(List<List<CategoryResponse>> listOfCategoriesList);

  /**
   * @param productCategoryResponses must not be null
   * @return master catalog
   */
  MasterCatalog convertToMasterCatalog(List<ProductCategoryResponse> productCategoryResponses);

  /**
   * @param categoryDetailResponse must not be null
   * @return master catalog
   */
  MasterCatalog convertToMasterCatalogFromDirectParentCategory(
      CategoryDetailResponse categoryDetailResponse);

  /**
   * @param attributeResponse must not be null
   * @return master data attribute
   */
  MasterDataAttribute convertToMasterDataAttribute(AttributeResponse attributeResponse);

  /**
   * @param productItemDetailResponse must not be null
   * @return masterDataItem
   */
  MasterDataItem convertToMasterDataItem(ProductItemDetailResponse productItemDetailResponse);

  Map<String, MasterDataItem> convertToMasterDataItems(
      Set<ProductItemResponse> productDetailResponses, String productCode);

  /**
   * @param productDetailResponse must not be null
   * @return master data product
   */
  MasterDataProduct convertToMasterDataProduct(ProductDetailResponse productDetailResponse);

  /**
   * @param price must not be null
   * @param itemSku must not be blank
   * @return price history
   */
  PriceHistory convertToPriceHistory(Price price, String itemSku);

  /**
   * @param product
   * @param item
   * @param itemCatalogs
   * @return
   * @throws Exception
   */
  ProductForTransactionVO convertToProductForTransaction(Product product, Item item,
      List<ItemCatalogVO> itemCatalogs);

  /**
   * Set ProductForTransactionVO with new master data class
   *
   * @param product
   * @param item
   * @param itemCatalogVOS
   * @param productMasterDataResponse
   * @return
   */
  ProductForTransactionVO convertToProductForTransactionNewMasterData(Product product, Item item,
    List<ItemCatalogVO> itemCatalogVOS, ProductMasterDataResponse productMasterDataResponse);

  /**
   * @param productCategoryResponses must not be null
   * @return sales catalogs
   */
  List<SalesCatalog> convertToSalesCatalogs(List<ProductCategoryResponse> productCategoryResponses);

  /**
   * @param salesCategoryReferenceResponse must not be null
   * @param umkmMerchant
   * @return sales catalogs
   */
  List<SalesCatalog> convertToSalesCatalogsFromDirectParentCategory(String requestId, String username,
      List<CategoryReferenceResponse> salesCategoryReferenceResponse,
      List<CategoryReferenceResponse> b2bSalesCategoryReferences, boolean umkmMerchant);

  ProductAttribute convertPristineAttributeToProductAttribute(PristineDataItem pristineDataItem);

  /**
   * @param masterDataProductAndItemsVO must not be null
   * @param itemCode must not be blank
   * @param itemSku must not be blank
   * @return OfferedSummaryVo
   */
  OfferedSummaryVo convertMasterDataProductAndItemsVoToOfferPageHeaderVo
  (MasterDataProductAndItemsVO masterDataProductAndItemsVO, String itemCode, String itemSku);

  /**
   * @param pristineItemDetailAndMappingVo must not be null
   * @return OfferedSummaryVo
   */
  OfferedSummaryVo convertPristineItemDetailAndMappingVoToOfferedSummaryVo
      (PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo);

  /**
   * @param param must not be null
   * @param product must not be null
   * @param item must not be blank
   * @return OfferedSummaryVo
   */
  OfferedSummaryVo convertProductToOfferedSummaryVo(MandatoryRequestParam param, Product product, Item item)
  throws Exception;
  /**
   * @param promoBundlingDetailResponseVO must not be null
   * @return promo bundling VO
   */
  ComboVO convertPromoBundlingDetailResponseVoToComboVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO);

  /**
   *
   * @param comboRuleVOs must not be null
   * @return Set of promoBundlingItemVO
   */
  List<ComboItemVO> convertComboRulesResponseToComboItemVO(
      List<ComboRuleVO> comboRuleVOs);

  /**
   * @param comboItemVO must not be null
   * @param product must not be null
   */
  void convertProductToComboItemVO(ComboItemVO comboItemVO, Product product);

  /**
   * @param comboItemVO must not be null
   * @param item must not be blank
   * @param product must not be null
   */
  void convertItemAndProductToComboItemVO(ComboItemVO comboItemVO, Item item, Product product);

  /**
   * @param itemInfoVO must not be null
   * @param item must not be null
   * @param product must not be null
   */
  void convertItemAndProductToItemInfoVO(ItemInfoVO itemInfoVO, Item item, Product product);

  /**
   * @param itemInfoVO must not be null
   * @param product must not be null
   */
  void convertProductToItemInfoVO(ItemInfoVO itemInfoVO, Product product);

  /**
   * @param promoBundlingDetailResponses must not be empty
   * @return list of promoBundlingDetailResponseVos
   */
  List<PromoBundlingDetailResponseVO> convertPromoBundlingDetailResponseToPromoBundlingDetailVo(
      List<PromoBundlingDetailResponse> promoBundlingDetailResponses
  );

  /**
   * @param promoBundlingByItemSkuAndItemCodesResponseVO must not be null
   * @param item must not be null
   * @return WholesaleVO
   */
  WholesaleVO convertPromoBundlingDetailResponseToWholesaleVO(PromoBundlingByItemSkuAndItemCodesResponseVO
      promoBundlingByItemSkuAndItemCodesResponseVO, Item item);

  /**
   * @param promoBundlingDetailResponseVO
   * @return List of wholesaleRuleVO
   */
  List<WholesaleRuleVO> convertPromoBundlingDetailResponseToWholesaleRuleVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO);

  /**
   * @param promoBundlingDetailResponseVO must not be null
   * @param priceByItemSku must not be null
   * @return PromoBundlingVO
   */
  PromoBundlingVO convertPromoBundlingDetailResponseVOToPromoBundlingVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO,
      Map<String, Price> priceByItemSku);

  /**
   * Generate mandatory request param
   *
   * @param businessPartnerChange
   * @return
   */
  MandatoryRequestParam generateMandatoryRequestParam(BusinessPartnerChange businessPartnerChange) throws Exception;

  /**
   * Generate item for need revision activation
   *
   * @param item
   * @param itemActivationRequest
   * @param itemPickupPoint
   * @return
   */
  boolean getItemForActivation(Item item, NeedCorrectionItemActivationRequest itemActivationRequest,
      ItemPickupPoint itemPickupPoint);

  /**
   * Generate item pickup point for need revision activation
   *
   * @param itemActivationRequest
   * @param itemPickupPoint
   * @param businessPartnerPickupPoint Default empty object instead of null
   * @return
   */
  ItemPickupPoint updateItemPickupPointOnNeedCorrectionActivation(NeedCorrectionItemActivationRequest itemActivationRequest,
      ItemPickupPoint itemPickupPoint, BusinessPartnerPickupPoint businessPartnerPickupPoint);

  /**
   * Convert from ProductResponse to Product
   * @param productEventModel
   * @return
   */
  Product convertToProduct(ProductEventModel productEventModel);

  /**
   * Convert to ProductScore from ProductScoreVo
   * @param productScoreVo must not be null
   * @return
   */
  ProductScore toProductScore(ProductScoreVo productScoreVo);

  /**
   * Convert from list of itemResponses to list of Item
   * @param itemEventModelList
   * @param productSku
   * @param needToOverrideL4DetailsFromL5
   * @return
   */
  List<Item> convertToListItem(List<ItemEventModel> itemEventModelList, String productSku,
      boolean needToOverrideL4DetailsFromL5);

  /**
   *
   * @param product
   * @return
   */
  ProductEventModel convertToProductEventModel(Product product);

  /**
   *
   * @param item
   * @return
   */
  ItemEventModel convertToItemEventModel(Item item);

  /**
   *
   * @param items
   * @return
   */
  List<ItemEventModel> convertToListItemEventModel(List<Item> items);

  /**
   *
   * @param productAndItemsVo
   * @return
   */
  ProductAndItemEventModel convertToProductAndItemEventModel(ProductAndItemsVO productAndItemsVo);

  /**
   *
   * @param productAndItemsVo
   * @param itemVos
   * @return
   */
  ProductAndItemEventModel convertToProductAndItemEventModel(ProductAndItemsVO productAndItemsVo,
      List<ItemVo> itemVos);

  /**
   * get Item summary list response
   * @param resultFromSolr
   * @param itemPickupPointMap
   * @param masterDataItems
   * @return
   */
   List<ItemSummaryListResponse> constructItemSummaryListResponse(List<ProductAndItemSolr> resultFromSolr, Map<String, ItemPickupPoint> itemPickupPointMap,
      Map<String, MasterDataItem> masterDataItems);

  /**
   *
   * @param items
   * @param productAndProductSkuMap
   * @param itemPickupPointMap
   * @return
   */
  List<ItemSummaryListResponse> constructItemSummaryListResponseUsingNewMasterData(List<Item> items,
      Map<String, Product> productAndProductSkuMap, Map<String, ItemPickupPoint> itemPickupPointMap);

  /**
   * Get sales category codes
   *
   * @param salesCatalogs
   * @return
   */
  List<String> getSalesCategoryCodesFromSalesCatalogs(List<SalesCatalog> salesCatalogs);

  /**
   * Override L4 details from L5 data
   *
   * @param items
   * @param itemPickupPoints
   */
  void overrideL4DetailsFromL5(List<Item> items, List<ItemPickupPoint> itemPickupPoints);

  /**
   * Override L5 details from L4 data
   *
   * @param items
   * @param itemPickupPoints
   */
  void overrideL5DetailsFromL4(List<Item> items, List<ItemPickupPoint> itemPickupPoints);

  /**
   * Convert to item pickup point change event model
   *
   * @param itemPickupPoint
   * @param pureCNCStatusChange
   */
  ItemPickupPointDataChangeEventModel convertToItemPickupPointChangeEventModel(ItemPickupPoint itemPickupPoint,
      Boolean pureCNCStatusChange);

  /**
   * Convert to BusinessPartnerPickupPoint
   *
   * @param pickupPointVO
   * @param businessPartnerCode
   */
  BusinessPartnerPickupPoint convertToBusinessPartnerPickupPoint(PickupPointVOEventModel pickupPointVO,
      String businessPartnerCode);

  /**
   * Convert to BusinessPartnerPickupPoint from pickupPointChange
   *
   * @param pickupPointChange
   * @param businessPartnerCode
   * @param businessPartnerPickupPoint
   * @return
   */
  BusinessPartnerPickupPoint convertToBusinessPartnerPickupPointFromPickupPointChange(PickupPointVOEventModel pickupPointChange,
      String businessPartnerCode, BusinessPartnerPickupPoint businessPartnerPickupPoint);

  /**
   * Converter to Product attribute details
   * @param productAttributeDomainEventModel
   * @return
   */
  ProductAttributeDetail toProductAttributeDetail(ProductAttributeDomainEventModel productAttributeDomainEventModel);

  /**
   * Converter to Product attribute details
   * @param productItemAttributeValueResponse
   * @return
   */
  ProductAttributeDetail toProductAttributeDetail(ProductItemAttributeValueResponse productItemAttributeValueResponse);

  /**
   * Convert List of Item to ItemLevel4ListingResponse
   *
   * @param itemSet
   * @param requestId
   * @param itemPickupPointByProductSkus
   * @param productCodeAndSharedProductMap
   * @return
   */
  List<ItemLevel4ListingResponse> convertItemToItemLevel4SummaryResponse(List<Item> itemSet, String requestId,
      List<ItemPickupPoint> itemPickupPointByProductSkus, Map<String, Boolean> productCodeAndSharedProductMap) throws Exception;

  /**
   * @param storeId
   * @param itemPickupPointList
   * @param items
   * @param productList
   * @param fetchB2bData
   * @param parentCategoriesFromDbAndCache
   * @param cncForWarehouseFeatureSwitch
   * @param fetchViewConfigByChannel
   * @return
   * @throws Exception
   */
  List<ItemLevel5Response> convertItemToItemLevel5Response(String storeId, List<ItemPickupPoint> itemPickupPointList,
      List<Item> items, List<Product> productList, boolean fetchB2bData,
      Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache, boolean cncForWarehouseFeatureSwitch, String fetchViewConfigByChannel) throws Exception;

  /**
   * get product transaction response v2
   * @param product
   * @param item
   * @param itemCatalogs
   * @return
   */
  ProductForTransactionVO convertToProductForTransactionV2(Product product, Item item,
      List<ItemCatalogVO> itemCatalogs);

  /**
   * generate product transaction vo response
   * @param productSkuWithoutMasterDataDetails
   * @param productAndItemsVOList
   * @param productMasterDataResponseMap
   * @return
   */
  List<ProductForTransactionVO> generateProductForTransactionVO(Set<String> productSkuWithoutMasterDataDetails,
      List<ProductAndItemsVO> productAndItemsVOList,
      Map<String, ProductMasterDataResponse> productMasterDataResponseMap);

  /**
   *
   * override defining attribute details from product to item
   * @param products
   *  @param items
   */
  void overrideDefiningAttributeDetailsFromL3ToL4(List<Product> products, List<Item> items);

  /**
   * get item basic details
   * @param itemPickupPoint
   * @param product
   * @param itemSku
   * @return
   */
  ItemBasicDetailResponse convertItemPickupPointsAndProductToItemBasicDetailResponse(
      Page<ItemPickupPoint> itemPickupPoint, Product product, String itemSku);

  /**
   * Convert to newly added L5 response
   * @param createdNew
   * @param items
   * @param itemPickupPoints
   * @return
   */
  ActivateNeedRevisionResponse convertActivateNeedRevisionResponseList(boolean createdNew, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints);

  /**
   * Convert Adjustment Product Change Api Response to Adjustment model
   * @param adjustmentProductBySkuAndPickupPointCode
   * @param adjustmentProduct
   * @return
   */
  List<AdjustmentProductChange> convertToAdjustmentProductChangeList(
    List<AdjustmentProductChangeResponseVO> adjustmentProductBySkuAndPickupPointCode,
    List<com.gdn.x.product.model.response.AdjustmentProductResponse> adjustmentProduct);

  /**
   * Convert Product to Duplicate product details response
   * @param product
   * @return
   */
  DuplicateProductDetailsResponse convertProductToDuplicateProductDetailsResponse(Product product);

  /**
   * Construct Product and Items View Response for PDP
   * @param product
   * @param itemMap
   * @param itemPickupPointMap
   * @return
   */
  ProductAndItemsSummaryResponseV2 constructProductAndItemsForView(Product product, Map<String, Item> itemMap, Map<String, ItemPickupPoint> itemPickupPointMap)
    throws Exception;

  /**
   * convert to master data product
   * @param productAndAttributeDetailResponse
   * @return
   */
  List<MasterDataProductAttribute> convertToMasterDataProductAttribute(ProductAndAttributeDetailResponse
      productAndAttributeDetailResponse);

  /**
   * convert to basic product and item dto
   * @param product
   * @param itemCatalogVOS
   * @param item
   * @return
   */
  BasicProductAndItemDTO convertToBasicProductAndItemDTO(Product product, List<ItemCatalogVO> itemCatalogVOS, BasicItemDTO item);

  /**
   * convert to ProductScoreVo
   *
   * @param productScore must not be null
   * @return
   */
  ProductScoreVo toProductScoreVoFromProductScore(ProductScore productScore);

  /**
   * convert to CombinedEditItemResponse
   *
   * @param editItemResponse must not be null
   * @return
   */
  CombinedEditItemResponse toCombinedEditItemResponse(EditItemResponse editItemResponse);

  /**
   * Set category hierarchy
   *
   * @param productBasicResponseList
   * @param parentCategoriesFromDbAndCache
   */
  void toSalesCategoryHierarchy(List<ProductBasicResponse> productBasicResponseList, Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache);

  /**
   * convert to odoo creation event model
   *
   * @param product
   * @param items
   * @return
   */
  OdooCreationEventModel convertToOdooCreationEventModel(Product product, List<ItemVo> items);
}
