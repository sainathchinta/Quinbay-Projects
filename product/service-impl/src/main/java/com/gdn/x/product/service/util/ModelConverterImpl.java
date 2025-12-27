package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.product.service.util.ResponseHelper.getItemViewConfigs;
import static com.gdn.x.product.service.util.ResponseHelper.setCncActivatedForBackward;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.rest.web.model.dto.DistributionInfoDTO;
import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;
import com.google.common.base.Functions;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAllowedAttributeValue;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.BuyableScheduleVo;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.DiscoverableScheduleVo;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemNameSkuVO;
import com.gdn.x.product.model.vo.ItemPickupPointDeleteRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.model.vo.PristineMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAttributeDetailVo;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.rest.web.model.ItemAndPickupPointBasicDetailResponse;
import com.gdn.x.product.rest.web.model.ItemDetailRestWeb;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.PristineProductAndItemsDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemsDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCategorySequenceDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleItemDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductItemDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddVariantRequest;
import com.gdn.x.product.rest.web.model.request.AllowedAttributeValueRequest;
import com.gdn.x.product.rest.web.model.request.AttributeRequest;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeValueRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SimpleProductListRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductAttributeDetailsDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductDTO;
import com.gdn.x.product.rest.web.model.response.ComboDetailResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.ItemAndBundlingInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailPageResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryPageResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.PristineProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.ProductInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailV2Response;
import com.gdn.x.product.rest.web.model.response.SimpleProductListResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.rest.web.model.response.SizeChartDataColumn;
import com.gdn.x.product.rest.web.model.response.SizeChartDataRow;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;
import com.gdn.x.product.service.api.AllowedAttributeValuesService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.api.util.ListUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableSet;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ModelConverterImpl implements ModelConverter {

  private static final String DEFAULT_CHANNEL = "DEFAULT";
  private static final int FIRST = 0;
  private static final Logger LOG = LoggerFactory.getLogger(ModelConverterImpl.class);
  private static final String LEVEL_2_INVENTORY_COUNT_RESPONSE_CANNOT_BE_NULL =
      "Level 2 inventory count response cannot be null";
  private static final String LEVEL_2_INVENTORY_LIST_OF_LEVEL_2_INVENTORY_REQUEST_REST_WEB_CANNOT_BE_NULL =
      "Level 2 inventory list of level 2 inventory request cannot be null";
  private static final String PRODUCT_CANNOT_BE_NULL = "Product cannot be null";
  private static final String SYSTEM_PARAMETER_MUST_NOT_BE_NULL =
      "System Parameter must not be null";
  private static final String SYSTEM_PARAMETER_REQUEST_MUST_NOT_BE_NULL =
      "System parameter request must not be null";
  private static final String PRODUCT_REQUEST_MUST_NOT_BE_NULL = "productRequest must not be null";
  private static final String PRODUCT_MUST_NOT_BE_NULL = "product must not be null";
  private static final String PRODUCT_ITEM_REQUEST_MUST_NOT_BE_NULL =
      "Product item request must not be null";
  private static final String ITEM_MUST_NOT_BE_NULL = "Item must not be null";
  private static final String PRICE_REQUEST_MUST_NOT_BE_NULL = "Price request must not be null";
  private static final String PRICE_MUST_NOT_BE_NULL = "Price must not be null";
  private static final String SALE_PRICE_REQUEST_MUST_NOT_BE_NULL =
      "Sale price request must not be null";
  private static final String SALE_PRICE_MUST_NOT_BE_NULL = "Sale price must not be null";
  private static final String PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL =
      "Product and items vo must not be null";
  private static final String SALES_CATALOG_MUST_NOT_BE_NULL = "sales catalog must not be null";
  private static final String SALES_CATALOG_REQUEST_MUST_NOT_BE_NULL =
      "sales catalog request must not be null";
  private static final String MASTER_CATALOG_REQUEST_MUST_NOT_BE_NULL =
      "master catalog request must not be null";
  private static final String MASTER_CATALOG_MUST_NOT_BE_NULL = "master catalog must not be null";
  private static final String PRODUCT_VIEW_CONFIG_REQUEST_MUST_NOT_BE_NULL =
      "productViewConfigRequest must not be null";
  private static final String PRODUCT_VIEW_CONFIG_MUST_NOT_BE_NULL =
      "productViewConfig must not be null";
  private static final String PRODUCT_DTO_MUST_NOT_BE_NULL = "productDTO must not be null";
  private static final String ITEM_DTO_MUST_NOT_BE_NULL = "itemDTO must not be null";
  private static final String PRODUCT_AND_ITEMS_DTO_MUST_NOT_BE_NULL =
      "productAndItemsDTO must not be null";
  private static final String ACTIVE_COMBO_REQUEST_MUST_NOT_BE_NULL =
      "activeComboRequest must not be null";
  private static final String WHOLESALE_VO_MUST_NOT_BE_NULL = "Wholesale VO must not be null";
  private static final String COMBO_RESPONSE_VO_MUST_NOT_BE_NULL =
      "comboResponseVO must not be null";
  private static final String COMBO_DETAIL_VO_MUST_NOT_BE_NULL = "Combo Detail Vo must not be null";
  private static final String TOTAL_PROMO_ITEM_VO_MUST_NOT_BE_NULL = "Total Promo Item VO must not be null";
  private static final List<String> GUARANTEE_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("garansi", "guarantee")));
  private static final List<String> GUARANTEE_DURATION_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("lama garansi")));
  private static final char SPACE = ' ';
  private static final String MASTER_DATA_ITEM_IMAGES = "master data item vo is null";
  private static final String OFFLINE_ITEM_LINE_PRICE = "offlineItemPrice VO is null";
  private static final String HYPHEN = "-";
  private static final String WAREHOUSE = "WH";
  private static final String MARKETPLACE = "MKT";

  @Autowired
  private GdnMapperHelper gdnMapperHelper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private ProductAndItemSolrConstructorService solrConstructorService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ListUtil listUtil;

  @Autowired
  private ItemService itemService;

  @Autowired
  private AllowedAttributeValuesService allowedAttributeValuesService;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${set.main.image.if.not.exists}")
  private boolean setMainImageIfNotExists;

  @Value("${set.default.product.type}")
  private boolean setDefaultProductType;

  @Value("${disable.preorder.delay.switch}")
  private boolean disablePreOrderDelay;

  @Value("${resize.image.path.removal}")
  private boolean resizeImageRemoval;

  @Value("${resize.image.path.list}")
  private String resizeImagePathList;

  @Value("${set.item.pickup.point.mark.for.delete}")
  private boolean setItemPickUpPointMarkForDeleteValue;

  @Value("${override.latefulfillment.by.product.type}")
  private boolean overrideLateFulfillmentByProductType;

  @Value("${subscription.l5.flow}")
  private boolean subscriptionAtL5Flow;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${remove.duplicate.product.attribute}")
  private boolean removeDuplicateProductAttribute;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${imei.attribute.code}")
  private String imeiAttributeCode;

  @Value("#{'${imei.attribute.allowed.values}'.split(',')}")
  private List<String> imeiAllowedValues;

  @Value("${set.special.attributes.in.pdp.api}")
  private boolean setSpecialAttributesInPDPApi;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Override
  public Item convertItemDTOToItem(ItemDTO itemDTO) {
    checkArgument(itemDTO != null, ModelConverterImpl.ITEM_DTO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(itemDTO, Item.class);
  }

  @Override
  public <B extends BaseResponse, L> List<B> convertListToResponse(Iterable<L> entities,
      Class<B> responseClass) {
    List<B> responses = new ArrayList<B>();
    for (L entity : entities) {
      responses.add(this.convertToResponse(entity, responseClass));
    }
    return responses;
  }

  @Override
  public Product convertProductDTOToProduct(ProductDTO productDTO) {
    checkArgument(productDTO != null, ModelConverterImpl.PRODUCT_DTO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(productDTO, Product.class);
  }

  @Override
  public <B, L extends BaseRequest> List<B> convertRequestListToModel(List<L> requests,
      Class<B> responseClass) {
    List<B> responses = new ArrayList<B>();
    for (L request : requests) {
      responses.add(this.convertRequestToModel(request, responseClass));
    }
    return responses;
  }

  @Override
  public <B, L extends BaseRequest> B convertRequestToModel(L request, Class<B> responseClass) {
    return this.gdnMapperHelper.mapBean(request, responseClass);
  }

  @Override
  public GdnRestSingleResponse<AddProductAndItemsResponse> convertToAddProductAndItemsResponse(
      String requestId, AddProductAndItemsResponseVo addProductAndItemsResponseVo) {
    return new GdnRestSingleResponse<AddProductAndItemsResponse>(this.gdnMapperHelper.mapBean(
        addProductAndItemsResponseVo, AddProductAndItemsResponse.class), requestId);
  }

  @Override
  public List<HandlingFeeRequest> convertToHandlingFeeRequestList(
      List<HandlingFeeRequestRestWeb> handlingFeeRequestRestWebList)
      throws ReflectiveOperationException {
    List<HandlingFeeRequest> handlingFeeRequestList = new ArrayList<HandlingFeeRequest>();
    for (HandlingFeeRequestRestWeb handlingFeeRequestRestWeb : handlingFeeRequestRestWebList) {
      HandlingFeeRequest handlingFeeRequest = new HandlingFeeRequest();
      PropertyUtils.copyProperties(handlingFeeRequest, handlingFeeRequestRestWeb);
      handlingFeeRequestList.add(handlingFeeRequest);
    }
    return handlingFeeRequestList;
  }

  @Override
  public List<UpsertOfflineItemRequest> convertToOfflineItemList(List<OfflineItemRequest> offlineItemRequestList,
      String merchantCode, String storeId) {
    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    for (OfflineItemRequest offlineItemRequest : offlineItemRequestList) {
     UpsertOfflineItemRequest upsertOfflineItemRequest = new UpsertOfflineItemRequest();
     BeanUtils.copyProperties(offlineItemRequest, upsertOfflineItemRequest);
     upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    }
    return upsertOfflineItemRequests;
  }

  @Override
  public List<OfflineItem> convertToOfflineItems(
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests, String merchantCode,
      String storeId) {
    List<OfflineItem> offlineItemList = new ArrayList<>();
    for (UpsertOfflineItemRequest upsertOfflineItemRequest : upsertOfflineItemRequests) {
      offlineItemList.add(constructOfflineItem(merchantCode, storeId, upsertOfflineItemRequest));
    }
    return offlineItemList;
  }

  public <T extends BaseRequest> OfflineItem constructOfflineItem(String merchantCode,
      String storeId, T upsertOfflineItemRequest) {
    OfflineItem offlineItem =
        this.gdnMapperHelper.mapBean(upsertOfflineItemRequest, OfflineItem.class);
    offlineItem.setMerchantCode(merchantCode);
    offlineItem.setStoreId(storeId);
    offlineItem.setListPrice(
        Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getOfferPrice()));
    return offlineItem;
  }

  @Override
  public HandlingFeeResponseRestWeb convertToHandlingFeeResponseRestWeb(
      HandlingFeeResponse handlingFeeResponse) throws ReflectiveOperationException {
    HandlingFeeResponseRestWeb handlingFeeResponseRestWeb = new HandlingFeeResponseRestWeb();
    PropertyUtils.copyProperties(handlingFeeResponseRestWeb, handlingFeeResponse);
    return handlingFeeResponseRestWeb;
  }

  @Override
  public Item convertToItem(ItemRequest itemRequest) {
    checkArgument(itemRequest != null, ModelConverterImpl.PRODUCT_ITEM_REQUEST_MUST_NOT_BE_NULL);
    Item item = this.gdnMapperHelper.mapBean(itemRequest, Item.class);
    item.setFreeSample(itemRequest.getFreeSample());
    return item;
  }

  @Override
  public GdnRestListResponse<ItemDetailRestWeb> convertToItemDetailRestWebGdnRestListResponse(
      List<ProductItemDetailVO> input, int page, long totalSize, String requestId) {
    ModelConverterImpl.LOG.debug("in:{}, {}, page: {}, size : {}", requestId, input, page,
        totalSize);
    List<ItemDetailRestWeb> content = new ArrayList<ItemDetailRestWeb>();
    if ((input != null) && !input.isEmpty()) {
      for (ProductItemDetailVO itemPrice : input) {
        content.add(new ItemDetailRestWeb.ItemDetailRestWebBuilder()
            .setItemName(itemPrice.getItemName()).setItemSku(itemPrice.getItemSku())
            .setMerchantCode(itemPrice.getMerchantCode()).build());
      }
    }
    GdnRestListResponse<ItemDetailRestWeb> result =
        new GdnRestListResponse<ItemDetailRestWeb>(null, null, true, content, new PageMetaData(
            content.size(), page, totalSize), requestId);
    ModelConverterImpl.LOG.debug("out:{}", result);
    return result;
  }

  @Override
  public Price convertToItemPrice(PriceRequest priceRequest) {
    checkArgument(priceRequest != null, ModelConverterImpl.PRICE_REQUEST_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(priceRequest, Price.class);
  }

  @Override
  public PriceDTO convertToItemPriceResponse(Price price) {
    checkArgument(price != null, ModelConverterImpl.PRICE_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(price, PriceDTO.class);
  }

  @Override
  public GdnRestListResponse<SimpleItemResponse> convertToItemPriceRestWebGdnRestListResponse(
      List<ItemPriceVO> input, String requestId) {
    ModelConverterImpl.LOG.debug("in:{}, {}", requestId, input);
    List<SimpleItemResponse> content = new ArrayList<SimpleItemResponse>();
    if ((input != null) && !input.isEmpty()) {
      for (ItemPriceVO itemPrice : input) {
        content.add(new SimpleItemResponse.SimpleItemResponseWebBuilder().setItemSku(itemPrice.getItemSku())
            .setOfferPrice(itemPrice.getOfferPrice()).setListPrice(itemPrice.getListPrice())
            .setBuyable(itemPrice.isBuyable()).setPickupPointCode(itemPrice.getPickupPointCode())
            .setDiscoverable(itemPrice.isDiscoverable()).build());
      }
    }
    GdnRestListResponse<SimpleItemResponse> result =
        new GdnRestListResponse<SimpleItemResponse>(null, null, true, content, new PageMetaData(
            content.size(), 1, content.size()), requestId);
    ModelConverterImpl.LOG.debug("out:{}", result);
    return result;
  }

  @Override
  public ItemResponse convertToItemResponse(Item item) {
    checkArgument(item != null, ModelConverterImpl.ITEM_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(item, ItemResponse.class);
  }

  @Override
  public DiscountPrice convertToItemSalePrice(DiscountPriceDTO salePriceRequest)
      throws ReflectiveOperationException {
    checkArgument(salePriceRequest != null, ModelConverterImpl.SALE_PRICE_REQUEST_MUST_NOT_BE_NULL);
    DiscountPrice salePrice = new DiscountPrice();
    PropertyUtils.copyProperties(salePrice, salePriceRequest);
    return salePrice;
  }

  @Override
  public DiscountPriceDTO convertToItemSalePriceResponse(DiscountPrice salePrice)
      throws ReflectiveOperationException {
    checkArgument(salePrice != null, ModelConverterImpl.SALE_PRICE_MUST_NOT_BE_NULL);
    DiscountPriceDTO salePriceResponse = new DiscountPriceDTO();
    PropertyUtils.copyProperties(salePriceResponse, salePrice);
    return salePriceResponse;
  }

  @Override
  public GdnRestListResponse<ItemSummaryResponse> convertToItemSummaryListResponse(String requestId, int page, int size,
      ItemSummaryPageResponseVo voObject) {
    checkArgument(voObject != null, "itemSummaryPageResponseVo must not be null");
    this.setPromoTypesForListOfItems(voObject);
    Map<String, ItemSummaryResponseVO> itemSummaryResponseVOMap = voObject.getItemSummaryResponses().stream()
        .collect(Collectors.toMap(ItemSummaryResponseVO::getItemSku, Function.identity()));
    ItemSummaryPageResponse pageResponses = this.gdnMapperHelper.mapBean(voObject, ItemSummaryPageResponse.class);
    pageResponses.getItemSummaryResponses().stream().forEach(itemSummaryResponse -> {
      itemSummaryResponse.setVersion(itemSummaryResponseVOMap.get(itemSummaryResponse.getItemSku()).getVersion());
      itemSummaryResponse.setLateFulfillment(
          CommonUtil.getLateFulfillmentFromProductType(itemSummaryResponse.getProductType(),
              overrideLateFulfillmentByProductType, itemSummaryResponse.isLateFulfillment()));
    });
    return new GdnRestListResponse<>(pageResponses.getItemSummaryResponses(),
        new PageMetaData(size, page, pageResponses.getTotalNum()), requestId);
  }

  @Override
  public GdnRestSingleResponse<ItemSummaryResponse> convertToItemSummarySingleResponse(
      String requestId, ItemSummaryPageResponseVo voObject) {
    checkArgument(voObject != null && voObject.getItemSummaryResponses() != null
        && !voObject.getItemSummaryResponses().isEmpty(), "itemSummary response is empty");
    ItemSummaryResponse response =
        this.gdnMapperHelper.mapBean(voObject.getItemSummaryResponses().get(0),
            ItemSummaryResponse.class);
    response.setSellerActivePromoBundlings(voObject.getItemSummaryResponses().get(0).getSellerActivePromoBundlings());
    return new GdnRestSingleResponse<ItemSummaryResponse>(response, requestId);
  }

  @Override
  public ItemViewConfig convertToItemViewConfig(ItemViewConfigRequest productViewConfigRequest) {
    checkArgument(productViewConfigRequest != null,
        ModelConverterImpl.PRODUCT_VIEW_CONFIG_REQUEST_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(productViewConfigRequest, ItemViewConfig.class);
  }

  @Override
  public Map<String, ItemViewConfig> convertToItemViewConfigMap(
      List<ItemViewConfigAndItemSkuRequest> productViewConfigListRequest) {
    Map<String, ItemViewConfig> map = new HashMap<>();
    for (ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest : productViewConfigListRequest) {
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest, itemViewConfig, "ItemDiscoverableScheduleDTO",
          "ItemBuyableScheduleDTO");
      if (Objects.nonNull(itemViewConfigAndItemSkuRequest.getItemBuyableSchedules())) {
        ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
        BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest.getItemBuyableSchedules(), itemBuyableSchedule);
        itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
      }
      if (Objects.nonNull(itemViewConfigAndItemSkuRequest.getItemDiscoverableSchedules())) {
        ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
        BeanUtils
            .copyProperties(itemViewConfigAndItemSkuRequest.getItemDiscoverableSchedules(), itemDiscoverableSchedule);
        itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
      }
      map.put(itemViewConfigAndItemSkuRequest.getItemSku(), itemViewConfig);
    }
    return map;
  }

  @Override
  public MasterCatalog convertToMasterCatalog(MasterCatalogRequest masterCatalogRequest) {
    checkArgument(masterCatalogRequest != null,
        ModelConverterImpl.MASTER_CATALOG_REQUEST_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(masterCatalogRequest, MasterCatalog.class);
  }

  @Override
  public MasterCatalogDTO convertToMasterCatalogResponse(MasterCatalog masterCatalog) {
    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    if (Objects.nonNull(masterCatalog)) {
      BeanUtils.copyProperties(masterCatalog, masterCatalogDTO);
      CategoryDTO categoryDTO = new CategoryDTO();
      if(Objects.nonNull(masterCatalog.getCategory())) {
        categoryDTO.setCategoryCode(masterCatalog.getCategory().getCategoryCode());
        categoryDTO.setCatgroupId(masterCatalog.getCategory().getCatgroupId());
      }
      masterCatalogDTO.setCategory(categoryDTO);
    }
    return masterCatalogDTO;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponse convertToMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo) {
    for (ProductAndItemsVO productAndItems : masterDataDetailResponseVo.getProductAndItems()) {
      Product product = productAndItems.getProduct();
      product.setMasterDataProduct(product.getMasterDataProduct());
      MasterDataProduct masterDataProduct;
      MasterDataItem masterDataItem;
      if (product.isSynchronized()) {
        masterDataProduct =
            masterDataDetailResponseVo.getMasterDataProducts().get(product.getProductCode());
        masterDataItem =
            masterDataDetailResponseVo.getMasterDataItems().get(
                productAndItems.getItems().get(0).getItemCode());
      } else {
        masterDataProduct = product.getMasterDataProduct();
        masterDataItem = productAndItems.getItems().get(0).getMasterDataItem();
      }
      product.getDescriptiveAttributes().addAll(
          this.fetchDescriptiveAttributes(masterDataProduct, false));
      for (Item item : productAndItems.getItems()) {
        if (!item.isSynchronized()) {
          item.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(
              item.getMasterDataItem(), product.getMasterDataProduct()));
        }
      }
      product.setSettlementType(this.productHelperService.getSettlementType(product,
          productAndItems.getItems().get(0)));
    }
    for (Entry<String, MasterDataItem> entry : masterDataDetailResponseVo.getMasterDataItems()
        .entrySet()) {
      this.masterDataConstructorService
          .constructItemDimensionFields(entry.getValue(), masterDataDetailResponseVo
              .getMasterDataProducts().get(entry.getValue().getProductCode()));
    }
    MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse =
        this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    setDocumentType(masterDataDetailWithProductAndItemsResponse);
    overrideLateFulfillmentInMasterDataProductAndItemsResponse(masterDataDetailWithProductAndItemsResponse);
    return masterDataDetailWithProductAndItemsResponse;
  }

  private void overrideLateFulfillmentInMasterDataProductAndItemsResponse(
      MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse) {
    if (overrideLateFulfillmentByProductType) {
      masterDataDetailWithProductAndItemsResponse.getProductAndItems().stream().filter(Objects::nonNull)
          .forEach(productAndItemsDTO -> {
            final ProductResponse product = productAndItemsDTO.getProduct();
            productAndItemsDTO.getItems().stream().filter(Objects::nonNull).forEach(itemResponse -> {
              itemResponse.setLateFulfillment(CommonUtil.getLateFulfillmentFromProductType(product.getProductType(),
                  overrideLateFulfillmentByProductType, itemResponse.getLateFulfillment()));
            });
          });
    }
  }

  @Override
  public MasterDataDetailWithProductAndItemResponse toMasterDataDetailWithProductAndItemResponse(
      MasterDataDetailWithProductAndItemResponseVo masterDataDetailWithProductAndItemResponseVo) {
    Product product = masterDataDetailWithProductAndItemResponseVo.getProduct();
    product.setMasterDataProduct(product.getMasterDataProduct());
    MasterDataProduct masterDataProduct;
    if (product.isSynchronized()) {
      masterDataProduct =
          masterDataDetailWithProductAndItemResponseVo.getMasterDataProducts().get(product.getProductCode());
    } else {
      masterDataProduct = product.getMasterDataProduct();
    }
    product.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(masterDataProduct, false));
    Item item = masterDataDetailWithProductAndItemResponseVo.getItem();
    if (!item.isSynchronized()) {
      item.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(
          item.getMasterDataItem(), product.getMasterDataProduct()));
    }
      product.setSettlementType(this.productHelperService.getSettlementType(product, item));
    for (Entry<String, MasterDataItem> entry : masterDataDetailWithProductAndItemResponseVo.getMasterDataItems()
        .entrySet()) {
      this.masterDataConstructorService
          .constructItemDimensionFields(entry.getValue(), masterDataDetailWithProductAndItemResponseVo
              .getMasterDataProducts().get(entry.getValue().getProductCode()));
    }
    MasterDataDetailWithProductAndItemResponse masterDataDetailWithProductAndItemResponse =
        this.gdnMapperHelper.mapBean(masterDataDetailWithProductAndItemResponseVo,
            MasterDataDetailWithProductAndItemResponse.class);
    if (Objects.nonNull(masterDataDetailWithProductAndItemResponse.getItem())) {
      masterDataDetailWithProductAndItemResponse.getItem().setLateFulfillment(
          CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
              masterDataDetailWithProductAndItemResponse.getItem().getLateFulfillment()));
    }
    return masterDataDetailWithProductAndItemResponse;
  }

  @Override
  public PristineMasterDataDetailResponse convertToPristineMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo) {

    PristineMasterDataDetailResponse response = null;
    for (ProductAndItemsVO productAndItems : masterDataDetailResponseVo.getProductAndItems()) {
      Product product = productAndItems.getProduct();
      product.setMasterDataProduct(product.getMasterDataProduct());
      MasterDataProduct masterDataProduct;
      MasterDataItem masterDataItem;
      if (product.isSynchronized()) {
        masterDataProduct =
            masterDataDetailResponseVo.getMasterDataProducts().get(product.getProductCode());
        masterDataItem =
            masterDataDetailResponseVo.getMasterDataItems().get(
                productAndItems.getItems().get(0).getItemCode());
      } else {
        masterDataProduct = product.getMasterDataProduct();
        masterDataItem = productAndItems.getItems().get(0).getMasterDataItem();
      }
      product.getDescriptiveAttributes().addAll(
          this.fetchDescriptiveAttributes(masterDataProduct, false));
      for (Item item : productAndItems.getItems()) {
        if (!item.isSynchronized()) {
          item.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(
              item.getMasterDataItem(), product.getMasterDataProduct()));
        }
      }
      product.setSettlementType(this.productHelperService.getSettlementType(product,
          productAndItems.getItems().get(0)));
    }
    for (Entry<String, MasterDataItem> entry : masterDataDetailResponseVo.getMasterDataItems()
        .entrySet()) {
      this.masterDataConstructorService
          .constructItemDimensionFields(entry.getValue(), masterDataDetailResponseVo
              .getMasterDataProducts().get(entry.getValue().getProductCode()));
    }
    String productName = masterDataDetailResponseVo.getProductAndItems().get(0).getItems().get(0)
        .getPristineDataItem().getPristineProductName();
    response = this.gdnMapperHelper.mapBean(masterDataDetailResponseVo,
        PristineMasterDataDetailResponse.class);
    if (response != null && response.getProductAndItems() != null) {
      response.getProductAndItems().get(0).getItems().get(0).setPristineProductName(productName);
      setDocumentType(response);
    }
    return response;
  }

  @Override
  public Product convertToProduct(ProductRequest productRequest) {
    checkArgument(productRequest != null, ModelConverterImpl.PRODUCT_REQUEST_MUST_NOT_BE_NULL);
    CommonUtil.overridePreOrderDateToJKT(productRequest);
    return this.gdnMapperHelper.mapBean(productRequest, Product.class);
  }

  @Override
  public ProductAndItemsResponse convertToProductAndItemsDTO(ProductAndItemsVO productAndItemsVO) {
    return convertToProductAndItemsDTO(productAndItemsVO, true);
  }

  @Override
  public ProductAndItemsResponse convertToProductAndItemsDTO(ProductAndItemsVO productAndItemsVO, boolean convertPreOrderDetails) {
    checkArgument(productAndItemsVO != null, ModelConverterImpl.PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    Product product = productAndItemsVO.getProduct();
    product.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(product.getMasterDataProduct(), false));
    if (product.getMasterDataProduct() != null) {
      product.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
    }

    for (Item item : productAndItemsVO.getItems()) {
      if (item.getMasterDataItem() != null) {
        item.setMasterDataItem(this.masterDataConstructorService
            .constructItemDimensionFields(item.getMasterDataItem(), product.getMasterDataProduct()));
        if (Objects.nonNull(item.getMasterDataItem()) && StringUtils.isNotBlank(item.getMasterDataItem().getHash())) {
          item.getMasterDataItem().setHash(StringUtils.SPACE);
        }
      }
    }
    product.setSettlementType(this.productHelperService.getSettlementType(product, productAndItemsVO.getItems().get(0)));
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    BeanUtils.copyProperties(productAndItemsVO, productAndItemsResponse, "product", "items");
    ProductResponse productResponse = gdnMapperHelper.mapBean(productAndItemsVO.getProduct(), ProductResponse.class);
    productResponse.setMarkForDelete(productAndItemsVO.getProduct().isMarkForDelete());
    if (Objects.nonNull(productAndItemsVO.getProduct().getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productAndItemsVO.getProduct().getProductScore(), productScoreResponse);
      productResponse.setProductScore(productScoreResponse);
    }
    productResponse.setPreOrder(checkPreOrder(productAndItemsVO.getProduct(), convertPreOrderDetails));
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());
    for (Item item: productAndItemsVO.getItems()) {
      ItemResponse itemResponse = gdnMapperHelper.mapBean(item, ItemResponse.class);
      itemResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(item));
      itemResponse.setMarkForDelete(item.isMarkForDelete());
      itemResponse.setVersion(item.getVersion());
      setWholesalePriceActivatedForItems(itemResponse, item);
      productAndItemsResponse.getItems().add(itemResponse);
    }
    setDocumentType(productAndItemsResponse);
    productAndItemsResponse.getItems().forEach(itemResponse -> itemResponse.setLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
            itemResponse.getLateFulfillment())));
    return setDisableUnSyncFlagToProductAndItemsResponse(productAndItemsVO.getProduct().getStoreId(),
        productAndItemsResponse);
  }

  private boolean checkPreOrderDateStatus(Date preOrderDate) {
    Date currentDate = new Date();
    try {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constants.DATE_FORMAT);
      currentDate = simpleDateFormat.parse(simpleDateFormat.format(currentDate));
    } catch (Exception e) {
      LOG.error("Error while parsing PreOrderDate : {} with error : ", currentDate, e);
      return true;
    }
    if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
      return true;
    }
    return false;
  }

  private void setDocumentType(ProductResponse productResponse) {
    try {
      if (Objects.nonNull(productResponse.getMasterCatalog()) && Objects.nonNull(
          productResponse.getMasterCatalog().getCategory())) {
        String categoryCode = productResponse.getMasterCatalog().getCategory().getCategoryCode();
        CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
        if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils.isNotBlank(
            categoryResponseByCategoryCode.getDocumentType())) {
          productResponse.setDocumentType(
              Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX)));
        }
      }
    } catch (Exception e) {
      LOG.error("Error when setting documentType for productCode : {} productSku : {}",
          productResponse.getProductCode(), productResponse.getProductSku(), e);
    }
  }

  private void setDocumentType(ProductAndItemsResponse productAndItemsResponse) {
    try {
      boolean isCategoryCodePresent =
          Optional.ofNullable(productAndItemsResponse.getProduct()).map(ProductResponse::getMasterCatalog)
              .map(MasterCatalogDTO::getCategory).isPresent();
      if (isCategoryCodePresent) {
        String categoryCode = productAndItemsResponse.getProduct().getMasterCatalog().getCategory().getCategoryCode();
        CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
        if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils.isNotBlank(
            categoryResponseByCategoryCode.getDocumentType())) {
          productAndItemsResponse.getProduct().setDocumentType(
              Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX)));
        }
      }
    } catch (Exception e) {
      LOG.error("Error when setting documentType for productCode : {} productSku : {}",
          productAndItemsResponse.getProduct().getProductCode(), productAndItemsResponse.getProduct().getProductSku(),
          e);
    }
  }

  private void setDocumentType(ProductAndItemInfoResponse productAndItemInfoResponse) {
    try {
      String categoryCode = productAndItemInfoResponse.getProduct().getMasterCatalog().getCategory().getCategoryCode();
      CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
      if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils
          .isNotBlank(categoryResponseByCategoryCode.getDocumentType())) {
        productAndItemInfoResponse.getProduct().setDocumentType(
            Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX)));
      }
    } catch (Exception e) {
      LOG.error("Error when setting documentType for productCode : {} productSku : {}",
          productAndItemInfoResponse.getProduct().getProductCode(),
          productAndItemInfoResponse.getProduct().getProductSku(), e);
    }
  }

  private void setDocumentType(
      MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse) {
    try {
      String categoryCode =
          masterDataDetailWithProductAndItemsResponse.getProductAndItems().get(0).getProduct().getMasterCatalog()
              .getCategory().getCategoryCode();
      CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
      if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils
          .isNotBlank(categoryResponseByCategoryCode.getDocumentType())) {
        masterDataDetailWithProductAndItemsResponse.getProductAndItems().forEach(
            productAndItemsDTO -> productAndItemsDTO.getProduct().setDocumentType(
                Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX))));
      }
    } catch (Exception e) {
      LOG.error("Error when setting documentType for productCode : {} ",
          masterDataDetailWithProductAndItemsResponse.getProductAndItems().get(0).getProduct().getProductCode(), e);
    }
  }

  private void setDocumentType(PristineMasterDataDetailResponse pristineMasterDataDetailResponse) {
    try {
      String categoryCode =
          pristineMasterDataDetailResponse.getProductAndItems().get(0).getProduct().getMasterCatalog().getCategory()
              .getCategoryCode();
      CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
      if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils
          .isNotBlank(categoryResponseByCategoryCode.getDocumentType())) {
        for (PristineProductAndItemsDTO productAndItemsDTO : pristineMasterDataDetailResponse.getProductAndItems()) {
          productAndItemsDTO.getProduct().setDocumentType(
              Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX)));
        }
      }
    } catch (Exception e) {
      LOG.error("Error when setting documentType for productCode : {} ",
          pristineMasterDataDetailResponse.getProductAndItems().get(0).getProduct().getProductCode(), e);
    }
  }

  private void setDocumentType(List<ProductForTransactionResponse> productForTransactionResponses) {
    for (ProductForTransactionResponse productForTransactionResponse : productForTransactionResponses) {
      try {
        String categoryCode = productForTransactionResponse.getItemDetail().getItemCatalogs().stream()
            .filter(itemCatalogDTO -> Constants.MASTER_CATALOG.equals(itemCatalogDTO.getCatalogId())).findFirst()
            .map(itemCatalogDTO -> itemCatalogDTO.getItemCategories().get(0).getCategoryId()).orElse(StringUtils.EMPTY);
        CategoryResponse categoryResponseByCategoryCode = getCategoryResponse(categoryCode);
        if (Objects.nonNull(categoryResponseByCategoryCode) && StringUtils
            .isNotBlank(categoryResponseByCategoryCode.getDocumentType())) {
          productForTransactionResponse.getItemDetail().setDocumentType(
              Arrays.asList(categoryResponseByCategoryCode.getDocumentType().split(Constants.COMMA_SPLIT_REGEX)));
        }
      } catch (Exception e) {
        LOG.error("Error when setting documentType for itemSku : {} ", productForTransactionResponse.getItemSku(), e);
      }
    }
  }

  private CategoryResponse getCategoryResponse(String categoryCode) {
    return productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode);
  }

  private void setWholesalePriceActivatedForItems(ItemResponse itemResponse, Item item) {
    if (item.isWholesalePriceExists()) {
      if (Optional.ofNullable(item.getActivePromoBundlings()).orElse(new HashSet<>())
          .contains(Constants.WHOLESALE_PRICE)) {
        itemResponse.setWholesalePriceActivated(true);
      } else {
        itemResponse.setWholesalePriceActivated(false);
      }
    }
  }

  private ProductAndItemsResponse setDisableUnSyncFlagToProductAndItemsResponse(
      String storeId, ProductAndItemsResponse productAndItemsResponse) {
    SystemParameter categoryCodeSystemParameter;
    try{
      categoryCodeSystemParameter =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId,
          Constants.CATEGORY_CODE_VARIABLE);
    } catch (ApplicationRuntimeException e) {
      return productAndItemsResponse;
    }
    List<String> categoryCodesForDisableUnSyncList = Arrays.asList(
        Optional.ofNullable(categoryCodeSystemParameter.getValue()).orElse(StringUtils.EMPTY)
            .split(Constants.COMMA_DELIMITER));
    for(ItemResponse itemResponse : productAndItemsResponse.getItems()) {
      if(Objects.nonNull(productAndItemsResponse.getProduct().getMasterCatalog())) {
        if(Objects.nonNull(productAndItemsResponse.getProduct().getMasterCatalog().getCategory())) {
          if (CollectionUtils.isNotEmpty(categoryCodesForDisableUnSyncList) && categoryCodesForDisableUnSyncList
              .contains(productAndItemsResponse.getProduct().getMasterCatalog().getCategory().getCategoryCode())) {
            itemResponse.setDisableUnSync(true);
          }
        }
      }
    }
    return productAndItemsResponse;
  }

  @Override
  public ProductResponse convertProductToProductResponse(Product product) {
    checkArgument(Objects.nonNull(product),
        ModelConverterImpl.PRODUCT_CANNOT_BE_NULL);
    product.getDescriptiveAttributes().addAll(
        this.fetchDescriptiveAttributes(product.getMasterDataProduct(), false));

    if (product.getMasterDataProduct() != null) {
      product.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
    }

    return this.gdnMapperHelper.mapBean(product, ProductResponse.class);
  }

  @Override
  public ProductAndItemInfoResponse convertToProductAndItemInfoResponse(
    ProductItemsVo productItemsVo) {
    checkArgument(Objects.nonNull(productItemsVo), PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    ProductVo productVo = productItemsVo.getProductVo();
    productVo.getDescriptiveAttributes().addAll(
        this.fetchDescriptiveAttributes(productVo.getMasterDataProduct(), false));
    productVo.setMasterCatalog(productVo.getMasterDataProduct().getMasterCatalog());
    ItemVo itemVo = productItemsVo.getItemVoList().get(FIRST);
    if (itemVo.getMasterDataItem() != null) {
      itemVo.setMasterDataItem(this.masterDataConstructorService
          .constructItemDimensionFields(itemVo.getMasterDataItem(), productVo.getMasterDataProduct()));
    }
    ProductInfoResponse productInfoResponse = gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class);
    ItemInfoResponse itemInfoResponse = gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class);
    Optional<ItemViewConfigDTO> itemViewConfigOpt = itemInfoResponse.getItemViewConfigs().stream()
        .filter(e -> DEFAULT_CHANNEL.equals(e.getChannel())).findFirst();
    itemInfoResponse.setChannelItemViewConfig(itemViewConfigOpt.orElse(null));
    Optional<PriceDTO> priceOpt = itemInfoResponse.getPrice().stream()
        .filter(e -> DEFAULT_CHANNEL.equals(e.getChannel())).findFirst();
    itemInfoResponse.setChannelPrice(priceOpt.orElse(null));
    productInfoResponse.setPreOrder(checkPreOrder(productItemsVo.getProductVo(), true));
    ProductAndItemInfoResponse productAndItemInfoResponse =
        new ProductAndItemInfoResponse(productInfoResponse, itemInfoResponse);
    setDocumentType(productAndItemInfoResponse);
    return productAndItemInfoResponse;
  }

  @Override
  public ProductItemsVo convertToProductAndItemsRequestVO(
      ProductAndItemActivationRequest productAndItemActivationRequest) {
    checkArgument(productAndItemActivationRequest != null, ModelConverterImpl.PRODUCT_AND_ITEMS_DTO_MUST_NOT_BE_NULL);
    ProductVo productVo = gdnMapperHelper.mapBean(productAndItemActivationRequest.getProduct(), ProductVo.class);
    List<ItemVo> itemsVos = productAndItemActivationRequest.getItems().stream()
        .map(itemDTO -> gdnMapperHelper.mapBean(itemDTO, ItemVo.class)).collect(toList());
    productVo.setTakenDown(productVo.isForceReview());
    ResponseHelper.setVideoUrlAndCoverImagePath(productAndItemActivationRequest.getProduct().getVideoAddEditRequest(), productVo);
    ProductItemsVo productItemsVo = new ProductItemsVo(productVo, itemsVos);
    productItemsVo.setItemVoList(listUtil.distinct(productItemsVo.getItemVoList(), ItemVo::getItemSku));
    PreOrder preOrder = new PreOrder();
    BeanUtils.copyProperties(productAndItemActivationRequest.getProduct().getPreOrder(), preOrder);
    if (Objects.nonNull(preOrder.getPreOrderDate())) {
      Calendar calendar = Calendar.getInstance();
      calendar.setTime(preOrder.getPreOrderDate());
      if (!disablePreOrderDelay) {
        calendar.add(Calendar.HOUR_OF_DAY, 7);
      }
      preOrder.setPreOrderDate(calendar.getTime());
    }
    productItemsVo.getProductVo().setPreOrder(preOrder);
    Map<String, List<ItemPickupPointActivationRequest>> itemPickupPointActivationRequestMap =
        productAndItemActivationRequest.getItems().stream()
            .collect(Collectors.toMap(ItemActivationRequest::getItemSku, ItemActivationRequest::getItemPickupPoints));
    productItemsVo.getItemVoList().forEach(itemsVo -> itemsVo.setItemPickupPointVoList(
        toItemPickupPointFromItemVo(itemsVo, itemPickupPointActivationRequestMap.get(itemsVo.getItemSku()),
           productVo.getProductSku(), productVo.isOnline())));
    productItemsVo.getItemVoList().forEach(itemsVo -> itemsVo.setOff2OnChannelActive(
      productItemsVo.getProductVo().isOff2OnChannelActive()));
    productItemsVo.getItemVoList().forEach(itemsVo -> itemsVo.setOmniChannelSku(itemsVo.getMerchantSku()));
    return productItemsVo;
  }

  @Override
  public List<ProductForTransactionResponse> convertToProductForTransactionResponse(
      List<ProductForTransactionVO> productVOs) {
    if (CollectionUtils.isEmpty(productVOs)) {
      return Collections.emptyList();
    }
    List<ProductForTransactionResponse> productForTransactionResponses =
        new ArrayList<ProductForTransactionResponse>();
    for (ProductForTransactionVO productVO : productVOs) {
      productForTransactionResponses.add(this.gdnMapperHelper.mapBean(productVO,
          ProductForTransactionResponse.class));
    }
    setDocumentType(productForTransactionResponses);
    return productForTransactionResponses;
  }

  @Override
  public List<ProductResponse> convertToProductResponseList(List<Product> products) {
    return products.stream().map(product -> convertToProductResponse(product)).collect(toList());
  }

  @Override
  public ProductResponse convertToProductResponse(Product product) {
    checkArgument(product != null, ModelConverterImpl.PRODUCT_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(product, ProductResponse.class);
  }

  @Override
  public GdnRestListResponse<ProductSummaryResponse> convertToProductSummaryResponse(
      Page<ProductAndItemSolr> productAndItems, String requestId, int page, int size) {
    List<ProductSummaryResponse> productSummaryResponses = new ArrayList<ProductSummaryResponse>();
    for (ProductAndItemSolr productAndItem : productAndItems.getContent()) {
      Product product = new Product();
      product.setProductSku(productAndItem.getProductSku());
      product.setSalesCatalogs(this.solrConstructorService.constructSalesCatalogs(productAndItem
          .getSalesCatalog()));
      product.setMasterCatalog(this.solrConstructorService.constructMasterCatalog(productAndItem
          .getMasterCatalog()));
      ProductSummaryResponse summaryResponse =
          this.gdnMapperHelper.mapBean(product, ProductSummaryResponse.class);
      summaryResponse.setProductName(productAndItem.getProductName());
      productSummaryResponses.add(summaryResponse);
    }
    return new GdnRestListResponse<ProductSummaryResponse>(productSummaryResponses,
        new PageMetaData(size, page, productAndItems.getTotalElements()), requestId);
  }

  @Override
  public ItemViewConfigDTO convertToProductViewConfigResponse(ItemViewConfig itemViewConfig) {
    checkArgument(itemViewConfig != null, ModelConverterImpl.PRODUCT_VIEW_CONFIG_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(itemViewConfig, ItemViewConfigDTO.class);
  }

  @Override
  public <B extends BaseResponse, L> B convertToResponse(L entity, Class<B> responseClass) {
    return this.gdnMapperHelper.mapBean(entity, responseClass);
  }

  @Override
  public SalesCatalog convertToSalesCatalog(SalesCatalogRequest salesCatalogRequest) {
    checkArgument(salesCatalogRequest != null,
        ModelConverterImpl.SALES_CATALOG_REQUEST_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(salesCatalogRequest, SalesCatalog.class);
  }

  @Override
  public SalesCatalogDTO convertToSalesCatalogResponse(SalesCatalog salesCatalog) {
    checkArgument(salesCatalog != null, ModelConverterImpl.SALES_CATALOG_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(salesCatalog, SalesCatalogDTO.class);
  }

  private SimpleProductItemDTO convertToSimpleProductItem(Product product, Item item) {
    SimpleProductItemDTO itemDetail = new SimpleProductItemDTO();

    if (CollectionUtils.isNotEmpty(product.getProductSpecialAttributes())) {
      String guaranteeAttributeName = StringUtils.EMPTY;
      String guaranteeDurationAttributeName = StringUtils.EMPTY;
      for (ProductSpecialAttribute productSpecialAttribute : product
          .getProductSpecialAttributes()) {
        if (productSpecialAttribute != null
            && StringUtils.isNotEmpty(productSpecialAttribute.getAttributeName())) {
          if (GUARANTEE_ATTRIBUTE_NAME
              .contains(productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeAttributeName = productSpecialAttribute.getAttributeValue();
          } else if (GUARANTEE_DURATION_ATTRIBUTE_NAME
              .contains(productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeDurationAttributeName = productSpecialAttribute.getAttributeValue();
          }
        }
      }

      if (StringUtils.isNotEmpty(guaranteeAttributeName)) {
        if (StringUtils.isNotEmpty(guaranteeDurationAttributeName)) {
          StringBuilder warrantyInfo = new StringBuilder(guaranteeAttributeName).append(SPACE)
              .append(guaranteeDurationAttributeName);
          itemDetail.setWarrantyInfo(warrantyInfo.toString());
        } else {
          itemDetail.setWarrantyInfo(guaranteeAttributeName);
        }
      }
    }

    itemDetail.setMerchantCode(product.getMerchantCode());
    itemDetail.setMerchantSku(item.getMerchantSku());
    itemDetail.setProductTypeCode(product.getProductType().getCode());
    itemDetail.setProductTypeName(product.getProductType().getDescription());
    itemDetail.setBrandName(product.getMasterDataProduct().getBrand());
    itemDetail.setProductCode(product.getProductCode());
    List<ItemCatalogVO> itemCatalogs = product.getItemCatalogs();
    if (itemCatalogs != null) {
      List<ItemCatalogDTO> itemCatalogDTO = itemCatalogs.stream()
          .map(e -> gdnMapperHelper.mapBean(e, ItemCatalogDTO.class)).collect(toList());
      itemDetail.setItemCatalogs(itemCatalogDTO);
    }
    itemDetail.setProductCatentryId(product.getProductCatentryId());
    itemDetail.setItemCatentryId(item.getItemCatentryId());
    itemDetail.setProductSku(product.getProductSku());
    itemDetail.setSettlementType(product.getSettlementType());
    itemDetail.setTicketTemplateCode(item.getTicketTemplateCode());
    itemDetail.setInstallationRequired(product.isInstallationRequired());
    itemDetail.setSettlementType(this.productHelperService.getSettlementType(product, item));
    itemDetail.setItemName(item.getMasterDataItem().getGeneratedItemName());
    itemDetail.setPickupPointCode(item.getPickupPointCode());
    item.setMasterDataItem(this.masterDataConstructorService
        .constructItemDimensionFields(item.getMasterDataItem(), product.getMasterDataProduct()));
    itemDetail.setItemLength(item.getMasterDataItem().getItemLength());
    itemDetail.setItemWidth(item.getMasterDataItem().getItemWidth());
    itemDetail.setItemHeight(item.getMasterDataItem().getItemHeight());
    itemDetail.setShippingWeight(item.getMasterDataItem().getItemDeliveryWeight());
    itemDetail.setItemWeight(item.getMasterDataItem().getItemWeight());
    boolean lateFulfillment = false;
    if (Boolean.TRUE.equals(item.isLateFulfillment())) {
      lateFulfillment = true;
    }
    itemDetail.setLateFulfillment(lateFulfillment);
    itemDetail.setDangerousLevel(item.getMasterDataItem().getDangerousLevel());
    itemDetail.setOff2OnChannelActive(item.isOff2OnChannelActive());
    return itemDetail;
  }

  @Override
  public List<SimpleProductRequestVo> convertToSimpleProductRequestVo(
      SimpleProductListRequest simpleProductListRequest) {
    List<SimpleProductRequestVo> requestVos = new ArrayList<SimpleProductRequestVo>();
    for (SimpleProductDTO simpleProductDto : simpleProductListRequest.getSimpleProducts()) {
      SimpleProductRequestVo requestVo = new SimpleProductRequestVo();
      requestVo.setProductCode(simpleProductDto.getProductCode());
      requestVo.setProductSku(simpleProductDto.getProductSku());
      requestVos.add(requestVo);
    }
    return requestVos;
  }

  @Override
  public List<ProductDetailResponse> convertToProductDetailResponse(
      List<ProductDetailVo> productDetailVos) {

    return productDetailVos.stream()
        .map(productDetailVo -> {
          ProductDetailResponse productDetailResponse = new ProductDetailResponse();
          BeanUtils.copyProperties(productDetailVo, productDetailResponse);
          productDetailResponse.setItemSkus(productDetailVo.getItemSkus());
          return productDetailResponse;
        })
        .collect(toList());
  }

  @Override
  public List<ActiveProductResponse> convertToActiveProductResponse(
      List<ActiveProductDetailVo> activeProductDetailVos) {
    List<ActiveProductResponse> activeProductResponses = new ArrayList<>();
    for (ActiveProductDetailVo activeProductDetailVo : activeProductDetailVos) {
      ActiveProductResponse activeProductResponse = toActiveProductResponse(activeProductDetailVo);
      if (CollectionUtils.isEmpty(activeProductResponse.getItemDetailResponses())) {
        activeProductResponses.add(activeProductResponse);
      } else {
        activeProductResponses.add(0, activeProductResponse);
      }
    }
    return activeProductResponses;
  }

  private ActiveProductResponse toActiveProductResponse(ActiveProductDetailVo productDetailVoList) {
    ActiveProductResponse activeProductResponse = new ActiveProductResponse();
    BeanUtils.copyProperties(productDetailVoList, activeProductResponse);
    List<ItemDetailResponse> detailResponses = new ArrayList<>();
    if(!CollectionUtils.isEmpty(productDetailVoList.getItemDetailVOList())) {
      for (ItemNameSkuVO itemNameSkuVO : productDetailVoList.getItemDetailVOList()) {
        ItemDetailResponse itemDetailResponse = new ItemDetailResponse();
        BeanUtils.copyProperties(itemNameSkuVO, itemDetailResponse);
        detailResponses.add(itemDetailResponse);
      }
    }
    if (Objects.nonNull(productDetailVoList.getItemAndPickupPointBasicDetailVo())) {
      ItemAndPickupPointBasicDetailResponse itemAndPickupPointBasicDetailResponse = new ItemAndPickupPointBasicDetailResponse();
      BeanUtils.copyProperties(productDetailVoList.getItemAndPickupPointBasicDetailVo(), itemAndPickupPointBasicDetailResponse);
      activeProductResponse.setItemAndPickupPointBasicDetailResponse(itemAndPickupPointBasicDetailResponse);
    }
    activeProductResponse.setItemDetailResponses(detailResponses);
    return activeProductResponse;
  }

  @Override
  public List<ItemSummaryResponse> convertToItemSummaryResponse(
      List<ItemInfoVO> itemInfoVOS) {
    return itemInfoVOS.stream()
        .map(itemInfoVO -> toItemSummaryResponse(itemInfoVO))
        .collect(toList());
  }

  private ItemSummaryResponse toItemSummaryResponse(ItemInfoVO itemInfoVO) {
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    MasterDataItemImageDTO masterDataItemImage = new MasterDataItemImageDTO();
    BeanUtils.copyProperties(itemInfoVO, itemSummaryResponse);
    masterDataItemImage.setLocationPath(itemInfoVO.getImageUrl());
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setSequence(0);
    itemSummaryResponse.setGeneratedItemName(itemInfoVO.getItemName());
    itemSummaryResponse.setMasterDataItemImages(Collections.singletonList(masterDataItemImage));
    return itemSummaryResponse;
  }

  @Override
  public SimpleProductResponse convertToSimpleProductResponse(Product product, Item item) {
    checkArgument(product != null, PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(item != null, ITEM_MUST_NOT_BE_NULL);
    String itemSku = item.getItemSku();
    try {
      SimpleProductItemDTO itemDetail = convertToSimpleProductItem(product, item);
      SimpleProductResponse simpleProduct = new SimpleProductResponse();
      simpleProduct.setItemDetail(itemDetail);
      simpleProduct.setItemSku(itemSku);
      simpleProduct.setItemCode(item.getItemCode());
      simpleProduct.setDescription(product.getMasterDataProduct().getDescription());
      if (CollectionUtils.isNotEmpty(item.getItemViewConfigs())) {
        simpleProduct.setBuyable(item.getItemViewConfigs().iterator().next().isBuyable());
      }
      if (CollectionUtils.isNotEmpty(item.getPrice())) {
        simpleProduct.setOfferPrice(item.getPrice().iterator().next().getOfferPrice());
      }
      simpleProduct.getItemDetail().setLateFulfillment(
          CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
              simpleProduct.getItemDetail().isLateFulfillment()));
      return simpleProduct;
    } catch (Exception e) {
      LOG.error("#convertToSimpleProductResponse failed on itemSku : {}, error : ", itemSku, e);
      return null;
    }
  }

  @Override
  public SimpleProductListResponse convertToSimpleProductResponses(List<Product> products) {
    List<SimpleProductDTO> responses = new ArrayList<SimpleProductDTO>();
    for (Product product : products) {
      SimpleProductDTO response = new SimpleProductDTO();
      response.setProductCode(product.getProductCode());
      response.setProductSku(product.getProductSku());
      responses.add(response);
    }
    return new SimpleProductListResponse(responses);
  }


  @Override
  public SystemParameter convertToSystemParameter(SystemParameterRequest systemParameterRequest,
      SystemParameter systemParameter) throws ReflectiveOperationException {
    checkArgument(systemParameterRequest != null,
        ModelConverterImpl.SYSTEM_PARAMETER_REQUEST_MUST_NOT_BE_NULL);
    PropertyUtils.copyProperties(systemParameter, systemParameterRequest);
    return systemParameter;
  }

  @Override
  public SystemParameterResponse convertToSystemParameterResponse(SystemParameter systemParameter)
      throws ReflectiveOperationException {
    checkArgument(systemParameter != null, ModelConverterImpl.SYSTEM_PARAMETER_MUST_NOT_BE_NULL);
    SystemParameterResponse systemParameterResponse = new SystemParameterResponse();
    PropertyUtils.copyProperties(systemParameterResponse, systemParameter);
    return systemParameterResponse;
  }

  @Override
  public UpdateItemSummaryRequestVo convertToUpdateItemSummaryVo(
      UpdateItemSummaryRequest updateItemSummaryRequest) {
    UpdateItemSummaryRequestVo requestVo =
        this.gdnMapperHelper.mapBean(updateItemSummaryRequest, UpdateItemSummaryRequestVo.class);
    if (requestVo.getItemViewConfigs() != null) {
      for (ItemViewConfig viewConfig : requestVo.getItemViewConfigs()) {
        if (StringUtils.isBlank(viewConfig.getChannel())) {
          viewConfig.setChannel(this.channelService.getDefaultChannel());
        }
      }
    }
    if (requestVo.getPrice() != null) {
      for (Price price : requestVo.getPrice()) {
        if (StringUtils.isBlank(price.getChannel())) {
          price.setChannel(this.channelService.getDefaultChannel());
        }
      }
    }
    return requestVo;
  }

  @Override
  public List<ItemListingUpdateRequestVo> convertToItemListingUpdateRequestVo(
      ItemListingUpdateRequest itemListingUpdateRequest) {
    List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos = new ArrayList<>();
    for (QuickEditUpdateRequest quickEditUpdateRequest : itemListingUpdateRequest.getQuickEditUpdateRequests()) {
      ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
      itemListingUpdateRequestVo.setItemSku(quickEditUpdateRequest.getItemSku());
      itemListingUpdateRequestVo.setMerchantSku(quickEditUpdateRequest.getSellerSku());
      itemListingUpdateRequestVo.setOff2OnChannelActive(quickEditUpdateRequest.getOff2OnActiveFlag());
      itemListingUpdateRequestVo.setPickupPointCode(quickEditUpdateRequest.getPickupPointCode());
      itemListingUpdateRequestVo.setWholesalePriceActivated(quickEditUpdateRequest.getWholeSaleActivated());
      itemListingUpdateRequestVo.setPrice(getPriceFromPriceDto(quickEditUpdateRequest.getPrice()));
      itemListingUpdateRequestVo.setItemViewConfigs(
          getItemViewConfigFromStatus(quickEditUpdateRequest.getStatus(), channelService.getDefaultChannel()));
      itemListingUpdateRequestVos.add(itemListingUpdateRequestVo);
    }
    return itemListingUpdateRequestVos;
  }

  private Set<Price> getPriceFromPriceDto(PriceDTO priceDTO) {
    Set<Price> prices = new HashSet<>();
    Price price = new Price();
    BeanUtils.copyProperties(priceDTO, price);
    price.setChannel(this.channelService.getDefaultChannel());
    prices.add(price);
    return prices;
  }

  private Set<ItemViewConfig> getItemViewConfigFromStatus(String status, String channel) {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    if (Constants.ONLINE.equals(status)) {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(true);
    } else if (Constants.OFFLINE.equals(status)) {
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(false);
    } else if (Constants.TEASER.equals(status)) {
      itemViewConfig.setDiscoverable(true);
      itemViewConfig.setBuyable(false);
    } else if (Constants.B2B.equals(status)) {
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(true);
    }
    itemViewConfig.setChannel(channel);
    itemViewConfigs.add(itemViewConfig);
    return itemViewConfigs;
  }

  public List<ProductAttributeDetail> fetchDescriptiveAttributeFromMasterDataProduct(
      List<MasterDataProductAttribute> masterDataProductAttributes, boolean fetchMultiValueAttributes) {
    List<ProductAttributeDetail> attributeDetails = new ArrayList<ProductAttributeDetail>();
    if (CollectionUtils.isNotEmpty(masterDataProductAttributes)) {
      for (MasterDataProductAttribute productAttribute : masterDataProductAttributes) {
        MasterDataAttribute attribute = productAttribute.getMasterDataAttribute();
        if (attribute != null && MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE
            .equals(attribute.getAttributeType()) && !attribute.isSkuValue() && !attribute.isVariantCreation()) {
          setDescriptiveAttributes(productAttribute, attributeDetails, attribute,
              MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.name());
        } else if (attribute != null && MasterDataAttributeType.PREDEFINED_ATTRIBUTE
            .equals(attribute.getAttributeType()) && !attribute.isSkuValue()) {
          setPredefinedValues(productAttribute, attributeDetails, attribute,
              MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name());
        } else if (productSuitabilityFeatureEnabled && fetchMultiValueAttributes && Objects.nonNull(
            attribute) && MasterDataAttributeType.DESCRIPTIVE_MULTIVALUE.equals(attribute.getAttributeType())
            && !attribute.isSkuValue()) {
          setDescriptiveAttributes(productAttribute, attributeDetails, attribute,
              MasterDataAttributeType.DESCRIPTIVE_MULTIVALUE.name());
        } else if (productSuitabilityFeatureEnabled && fetchMultiValueAttributes && Objects.nonNull(attribute)
            && MasterDataAttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())
            && !attribute.isSkuValue()) {
          setPredefinedValues(productAttribute, attributeDetails, attribute,
              MasterDataAttributeType.PREDEFINED_MULTIVALUE.name());
        }
      }
    }
    return attributeDetails;
  }

  private static void setDescriptiveAttributes(MasterDataProductAttribute productAttribute,
      List<ProductAttributeDetail> attributeDetails, MasterDataAttribute attribute, String attributeType) {
    for (MasterDataProductAttributeValue attributeValue : Optional.ofNullable(
        productAttribute.getMasterDataProductAttributeValues()).orElse(new ArrayList<>())) {
      if (!attributeValue.isMarkForDelete()) {
        ProductAttributeDetail productAttributeDetail =
            new ProductAttributeDetail(attribute.getAttributeCode(), attribute.getAttributeName(),
                attributeValue.getDescriptiveAttributeValue(), attribute.isMustShow());
        productAttributeDetail.setAttributeType(attributeType);
        attributeDetails.add(productAttributeDetail);
      }
    }
  }

  private static void setPredefinedValues(MasterDataProductAttribute productAttribute,
      List<ProductAttributeDetail> attributeDetails, MasterDataAttribute attribute, String attributeType) {
    for (MasterDataProductAttributeValue attributeValue : Optional.ofNullable(
        productAttribute.getMasterDataProductAttributeValues()).orElse(new ArrayList<>())) {

      if (!attributeValue.isMarkForDelete()) {
        PredefinedAllowedAttributeValue predefinedValue = attributeValue.getPredefinedAllowedAttributeValue();
        String value = Optional.ofNullable(predefinedValue).map(PredefinedAllowedAttributeValue::getValue).orElse(null);
        String valueEn =
            Optional.ofNullable(predefinedValue).map(PredefinedAllowedAttributeValue::getValueEn).orElse(null);
        attributeDetails.add(
            new ProductAttributeDetail(attribute.getAttributeCode(), attribute.getAttributeName(), value,
                attribute.isMustShow(), valueEn, attributeType));
      }
    }
  }

  private List<ProductAttributeDetail> fetchDescriptiveAttributes(
      MasterDataProduct masterDataProduct, boolean fetchMultiValueAttributes) {
    List<ProductAttributeDetail> attributeDetails = new ArrayList<>();
    if (Objects.nonNull(masterDataProduct)) {
      attributeDetails = this.fetchDescriptiveAttributeFromMasterDataProduct(
          masterDataProduct.getMasterDataProductAttributes(), fetchMultiValueAttributes);
    }
    return attributeDetails;

  }

  @Override
  public List<SimpleProductsAndItemsResponse> convertToSimpleProductsAndItemsDTO(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo) {

    List<SimpleProductsAndItemsResponse> simpleProductsAndItemsDTOList = null;
    if (masterDataDetailWithProductAndItemsResponseVo != null) {
      simpleProductsAndItemsDTOList = new ArrayList<>();
      List<ProductAndItemsVO> productAndItemsDTOList =
          masterDataDetailWithProductAndItemsResponseVo.getProductAndItems();

      if (CollectionUtils.isNotEmpty(productAndItemsDTOList)) {
        for (ProductAndItemsVO productAndItemsVO : productAndItemsDTOList) {
          SimpleProductsAndItemsResponse simpleProductsAndItemsDTO =
              new SimpleProductsAndItemsResponse();
          SimpleProductDTO simpleProductDTO = new SimpleProductDTO();
          List<SimpleItemDTO> simpleItemDTOList = new ArrayList<>();
          Product product = productAndItemsVO.getProduct();
          if (product != null) {
            simpleProductDTO.setProductCode(product.getProductCode());
            simpleProductDTO.setProductSku(product.getProductSku());
            simpleProductDTO.setSynchronized(product.isSynchronized());
          }
          simpleProductsAndItemsDTO.setSimpleProductDTO(simpleProductDTO);
          if (CollectionUtils.isNotEmpty(productAndItemsVO.getItems())) {
            for (Item item : productAndItemsVO.getItems()) {
              SimpleItemDTO simpleItemDTO = new SimpleItemDTO();
              if (CollectionUtils.isNotEmpty(item.getItemViewConfigs())) {
                simpleItemDTO
                    .setDiscoverable(item.getItemViewConfigs().iterator().next().isDiscoverable());
              }
              simpleItemDTO.setSynchronized(item.isSynchronized());
              simpleItemDTO.setItemsSkus(item.getItemSku());
              if (item.getPristineDataItem() != null) {
                simpleItemDTO.setPristineId(item.getPristineDataItem().getPristineId());
              }
              simpleItemDTOList.add(simpleItemDTO);
            }
          }
          simpleProductsAndItemsDTO.setSimpleItemDTOList(simpleItemDTOList);
          simpleProductsAndItemsDTOList.add(simpleProductsAndItemsDTO);
        }
      }
    }
    return simpleProductsAndItemsDTOList;
  }


  @Override
  public List<SimpleProductMasterDataDetailResponse> convertToSimpleProductMasterDataDetailResponse(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo)
      throws Exception {
    List<SimpleProductMasterDataDetailResponse> response = new ArrayList<>();
    if (masterDataDetailWithProductAndItemsResponseVo != null) {

      for (ProductAndItemsVO productAndItemsVO : masterDataDetailWithProductAndItemsResponseVo
          .getProductAndItems()) {
        SimpleProductMasterDataDetailResponse simpleProductMasterDataDetailResponse =
            new SimpleProductMasterDataDetailResponse();
        simpleProductMasterDataDetailResponse
            .setProductSku(productAndItemsVO.getProduct().getProductSku());
        simpleProductMasterDataDetailResponse
            .setProductCode(productAndItemsVO.getProduct().getProductCode());
        MasterDataProduct masterDataProduct =
            masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts()
                .get(simpleProductMasterDataDetailResponse.getProductCode());
        if (productAndItemsVO.getProduct().getSalesCategorySequences() != null) {
          simpleProductMasterDataDetailResponse.setSalesCategorySequenceDTOList(
              convertSalesCategorySequenceListToDto(productAndItemsVO.getProduct().getSalesCategorySequences()));
        }
        if (masterDataProduct != null) {
          simpleProductMasterDataDetailResponse.setBrand(masterDataProduct.getBrand());
          simpleProductMasterDataDetailResponse.setProductName(masterDataProduct.getProductName());
        }
        List<ItemCatalogDTO> itemCatalogDTOs = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(productAndItemsVO.getProduct().getItemCatalogs())) {
          for (ItemCatalogVO itemCatalogVO : productAndItemsVO.getProduct().getItemCatalogs()) {
            ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
            PropertyUtils.copyProperties(itemCatalogDTO, itemCatalogVO);
            itemCatalogDTOs.add(itemCatalogDTO);
          }
        }
        simpleProductMasterDataDetailResponse.setItemCatalogDTOList(itemCatalogDTOs);
        response.add(simpleProductMasterDataDetailResponse);
      }
    }
    return response;
  }

  private List<SalesCategorySequenceDTO> convertSalesCategorySequenceListToDto(
      List<SalesCategorySequence> salesCategorySequenceList)
      throws IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    SalesCategorySequenceDTO salesCategorySequenceDTO;
    List<SalesCategorySequenceDTO> salesCategorySequenceDTOs = new ArrayList<>();

    for (SalesCategorySequence salesCategorySequence : salesCategorySequenceList) {
      salesCategorySequenceDTO = new SalesCategorySequenceDTO();
      PropertyUtils.copyProperties(salesCategorySequenceDTO, salesCategorySequence);
      salesCategorySequenceDTOs.add(salesCategorySequenceDTO);
    }
    return salesCategorySequenceDTOs;
  }

  @Override
  public List<SimplePristineProductResponse> convertToSimplePristineProductResponse(
      List<SimplePristineProductRequestVo> simplePristineProductRequestVoList) throws Exception {

    List<SimplePristineProductResponse> response = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(simplePristineProductRequestVoList)) {
      response = new ArrayList<>();
      for (SimplePristineProductRequestVo simplePristineProductRequestVo :
          simplePristineProductRequestVoList) {
        SimplePristineProductResponse simplePristineProductResponse = new SimplePristineProductResponse();
        simplePristineProductResponse.setPristineId(simplePristineProductRequestVo.getPristineId());
       PropertyUtils.copyProperties(simplePristineProductResponse,simplePristineProductRequestVo);
        response.add(simplePristineProductResponse);
      }
    }
    return response;
  }

  @Override
  public PristineDataItem convertToPristineDataItem(PristineDataItemDto pristineDataItemDto)
      throws ReflectiveOperationException {
    PristineDataItem pristineDataItem = new PristineDataItem();
    PropertyUtils.copyProperties(pristineDataItem, pristineDataItemDto);
    return pristineDataItem;

  }

  @Override
  public PristineMasterDataDetailWithProductAndItemsResponse convertToPristineMasterDataDetailResponse(
      PristineMasterDataDetailWithProductAndItemsResponseVo pristineMasterDataDetailResponseVo) {
    for (ProductAndItemsVO productAndItems : pristineMasterDataDetailResponseVo.getProductAndItems()) {
      Product product = productAndItems.getProduct();
      MasterDataProduct masterDataProduct;
      MasterDataItem masterDataItem;
      if (product.isSynchronized()) {
        masterDataProduct =
            pristineMasterDataDetailResponseVo.getMasterDataProducts().get(product.getProductCode());
        masterDataItem =
            pristineMasterDataDetailResponseVo.getMasterDataItems().get(
                productAndItems.getItems().get(0).getItemCode());
      } else {
        masterDataProduct = product.getMasterDataProduct();
        masterDataItem = productAndItems.getItems().get(0).getMasterDataItem();
      }
      product.getDescriptiveAttributes().addAll(
          this.fetchDescriptiveAttributes(masterDataProduct, false));
      for (Item item : productAndItems.getItems()) {
        if (!item.isSynchronized()) {
          item.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(
              item.getMasterDataItem(), product.getMasterDataProduct()));
        }
      }
      product.setSettlementType(this.productHelperService.getSettlementType(product,
          productAndItems.getItems().get(0)));
    }
    for (Entry<String, MasterDataItem> entry : pristineMasterDataDetailResponseVo.getMasterDataItems()
        .entrySet()) {
      this.masterDataConstructorService
          .constructItemDimensionFields(entry.getValue(), pristineMasterDataDetailResponseVo
              .getMasterDataProducts().get(entry.getValue().getProductCode()));
    }
    return this.gdnMapperHelper.mapBean(pristineMasterDataDetailResponseVo,
        PristineMasterDataDetailWithProductAndItemsResponse.class);
  }

  @Override
  public PristineProductAndItemsResponse convertToPristineProductAndItemsResponse(
      PristineProductAndItemsResponseVO pristineProductAndItemsResponseVO) {
    return this.gdnMapperHelper.mapBean(pristineProductAndItemsResponseVO,
        PristineProductAndItemsResponse.class);
  }

  @Override
  public List<OfflineItemResponseDetail> convertItemToOfflineProductResponse(
      List<String> merchantSkus, List<Item> items, Map<String, ProductType> productTypeByProductSkuMap) {

    List<OfflineItemResponseDetail> offlineProducts = new ArrayList<>();

    Map<String, Long> merchantSkuCounterMap = items.stream().collect(
        Collectors.groupingBy(Item::getMerchantSku, Collectors.counting()));

    Map<String, String> itemSkuByMerchantSkuMap = items.stream().collect(HashMap::new,
        (map, item) -> map.put(item.getMerchantSku(), item.getItemSku()), HashMap::putAll);

    Map<String, String> productSkuByMerchantSkuMap = items.stream().collect(HashMap::new,
        (map, item) -> map.put(item.getMerchantSku(), item.getProductSku()), HashMap::putAll);

    for (String merchantSku : merchantSkus){
      OfflineItemResponseDetail offlineProduct = new OfflineItemResponseDetail();
      offlineProduct.setMerchantSku(merchantSku);

      String productSku = productSkuByMerchantSkuMap.get(merchantSku);
      boolean isBopisProduct = productSku != null &&
          ProductType.BOPIS.equals(productTypeByProductSkuMap.get(productSku));

      if (merchantSkuCounterMap.get(merchantSku) == null) {
        offlineProduct.setErrorCode(ProductErrorCodesEnum.MERCHANT_SKU_NOT_FOUND.getCode());
      } else if (merchantSkuCounterMap.get(merchantSku) > 1) {
        offlineProduct.setErrorCode(ProductErrorCodesEnum.DUPLICATE_MERCHANT_SKU.getCode());
      } else if (isBopisProduct){
        offlineProduct.setErrorCode(ProductErrorCodesEnum.BOPIS_PRODUCT_CANNOT_BE_CONVERTED_TO_CNC.getCode());
      } else {
        offlineProduct.setItemSku(itemSkuByMerchantSkuMap.get(merchantSku));
      }
      offlineProducts.add(offlineProduct);
    }

    return offlineProducts;
  }

  private MasterDataAttribute convertToMasterDataAttribute(AttributeRequest attributeRequest) {
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(attributeRequest.getAttributeCode());
    masterDataAttribute.setAttributeName(attributeRequest.getName());
    masterDataAttribute.setAttributeType(
        MasterDataAttributeType.valueOf(String.valueOf(attributeRequest.getAttributeType())));
    masterDataAttribute
        .setDescription(this.convertBytesToString(attributeRequest.getDescription()));
    masterDataAttribute.setSearchable(attributeRequest.isSearchAble());
    masterDataAttribute.setSkuValue(attributeRequest.isSkuValue());
    masterDataAttribute.setStoreId(attributeRequest.getStoreId());
    return masterDataAttribute;
  }

  private String convertBytesToString(byte[] bytes) {
    if (bytes != null) {
      return new String(bytes);
    } else {
      return null;
    }
  }

  @Override
  public MasterDataProductAttribute convertProductAttributeRequestToMasterDataProductAttribute(
      ProductAttributeRequest productAttributeRequest) {
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    masterDataProductAttribute.setMasterDataAttribute(
        this.convertToMasterDataAttribute(productAttributeRequest.getAttribute()));
    masterDataProductAttribute.setOwnedByProductItem(productAttributeRequest.isOwnByProductItem());
    masterDataProductAttribute.setSequence(productAttributeRequest.getSequence());

    List<MasterDataProductAttributeValue> masterDataProductAttributeValues = new ArrayList<>();
    for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest
        .getProductAttributeValues()) {
      MasterDataProductAttributeValue masterDataProductAttributeValue =
          new MasterDataProductAttributeValue();
      AllowedAttributeValueRequest allowedAttributeValue =
          productAttributeValueRequest.getAllowedAttributeValue();
      if (allowedAttributeValue != null) {
        String allowedAttributeCode = allowedAttributeValue.getAllowedAttributeCode();
        masterDataProductAttributeValue.setAllowedAttributeValueCode(allowedAttributeCode);
        MasterDataAllowedAttributeValue masterDataAllowedAttributeValue =
            new MasterDataAllowedAttributeValue();
        masterDataAllowedAttributeValue.setAllowedAttributeValueCode(allowedAttributeCode);
        masterDataAllowedAttributeValue.setValue(allowedAttributeValue.getValue());
        masterDataAllowedAttributeValue.setSequence(allowedAttributeValue.getSequence());
        masterDataProductAttributeValue.setAllowedAttributeValue(masterDataAllowedAttributeValue);
      }
      if (productAttributeValueRequest.getPredefinedAllowedAttributeValue() != null) {
        masterDataProductAttributeValue
            .setPredefinedAllowedAttributeValueCode(productAttributeValueRequest
                .getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
        PredefinedAllowedAttributeValue predefined = new PredefinedAllowedAttributeValue();
        predefined.setPredefinedAllowedAttributeCode(productAttributeValueRequest
            .getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
        predefined.setSequence(
            productAttributeValueRequest.getPredefinedAllowedAttributeValue().getSequence());
        predefined
            .setValue(productAttributeValueRequest.getPredefinedAllowedAttributeValue().getValue());
        masterDataProductAttributeValue.setPredefinedAllowedAttributeValue(predefined);
      }
      masterDataProductAttributeValue.setDescriptiveAttributeValue(
          productAttributeValueRequest.getDescriptiveAttributeValue());
      masterDataProductAttributeValue
          .setMarkForDelete(productAttributeValueRequest.isMarkForDelete());
      if (productAttributeValueRequest.getDescriptiveAttributeValueType() != null) {
        masterDataProductAttributeValue
            .setDescriptiveAttributeValueType(DescriptiveAttributeValueType
                .valueOf(productAttributeValueRequest.getDescriptiveAttributeValueType().name()));
      }
      masterDataProductAttributeValues.add(masterDataProductAttributeValue);
      masterDataProductAttribute
          .setMasterDataProductAttributeValues(masterDataProductAttributeValues);
    }
    return masterDataProductAttribute;
  }

  public List<ProductAndItemsResponse> convertToProductAndItemsDTOs(List<ProductAndItemsVO> productAndItemsVOs) {
    List<ProductAndItemsResponse> productAndItemsResponses = new ArrayList<>();

    for (ProductAndItemsVO productAndItemsVO : productAndItemsVOs) {
      ProductAndItemsResponse productAndItemsResponse = convertToProductAndItemsDTO(productAndItemsVO);
      productAndItemsResponses.add(productAndItemsResponse);
    }

    return productAndItemsResponses;
  }

  @Override
  public ActiveComboRequestVO convertActiveComboRequestToActiveComboRequestVO(
      ActiveComboRequest activeComboRequest) {
    checkArgument(activeComboRequest != null,
        ModelConverterImpl.ACTIVE_COMBO_REQUEST_MUST_NOT_BE_NULL);

    return this.gdnMapperHelper
        .mapBean(activeComboRequest, ActiveComboRequestVO.class);
  }

  @Override
  public WholesaleResponse convertWholesaleVOToWholesaleResponse(WholesaleVO wholesaleVO){
    checkArgument(wholesaleVO != null, ModelConverterImpl.WHOLESALE_VO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(wholesaleVO, WholesaleResponse.class);
  }

  @Override
  public ComboResponse convertComboResponseVOToComboResponse(ComboResponseVO comboResponseVO) {
    checkArgument(comboResponseVO != null, ModelConverterImpl.COMBO_RESPONSE_VO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(comboResponseVO, ComboResponse.class);
  }

  @Override
  public ComboDetailResponse convertComboDetailVoToComboDetailResponse(
      ComboDetailVo comboDetailVo) {
    checkArgument(comboDetailVo != null, ModelConverterImpl.COMBO_DETAIL_VO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(comboDetailVo, ComboDetailResponse.class);
  }

  @Override
  public ItemAndBundlingInfoResponse convertPromoItemVOToPromoItemResponse(ItemAndBundlingInfoVO itemAndBundlingInfoVO) {
    checkArgument(itemAndBundlingInfoVO != null, ModelConverterImpl.TOTAL_PROMO_ITEM_VO_MUST_NOT_BE_NULL);
    return this.gdnMapperHelper.mapBean(itemAndBundlingInfoVO, ItemAndBundlingInfoResponse.class);
  }

  @Override
  public List<ItemPriceResponse> convertItemPriceVoToItemPriceListResponse(List<ItemPriceVO> itemPriceVOS){
    return itemPriceVOS.stream().map(itemPriceVO -> {
      return new ItemPriceResponse.ItemPriceResponseBuilder()
          .setBuyable(itemPriceVO.isBuyable())
          .setItemSku(itemPriceVO.getItemSku())
          .setListPrice(itemPriceVO.getListPrice())
          .setOfferPrice(itemPriceVO.getOfferPrice())
          .setMerchantCode(itemPriceVO.getMerchantCode()).build();
    }).collect(toList());
  }

  @Override
  public OfficialStoreRequestVO toOfficialStoreRequestVO(
      ProductDetailRequest productDetailRequest) {
    OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
    officialStoreRequestVO.setBrands(productDetailRequest.getBrands());
    officialStoreRequestVO.setMerchantCodes(productDetailRequest.getMerchantCodes());
    officialStoreRequestVO.setProductName(productDetailRequest.getProductName());
    officialStoreRequestVO.setProductSku(productDetailRequest.getProductSku());
    officialStoreRequestVO.setCategoryCode(productDetailRequest.getCategoryCode());
    officialStoreRequestVO.setChannelName(productDetailRequest.getChannelName());
    officialStoreRequestVO.setDiscoverable(productDetailRequest.getDiscoverable());
    officialStoreRequestVO.setBuyable(productDetailRequest.getBuyable());
    officialStoreRequestVO.setArchived(productDetailRequest.getArchived());
    officialStoreRequestVO.setMinPrice(Objects.nonNull(productDetailRequest.getMinPrice()) ?
        productDetailRequest.getMinPrice() : 0);
    officialStoreRequestVO.setMaxPrice(Objects.nonNull(productDetailRequest.getMaxPrice()) ?
        productDetailRequest.getMaxPrice() : Double.MAX_VALUE);
    officialStoreRequestVO.setOff2OnChannelActive(productDetailRequest.getOff2OnChannelActive());
    return officialStoreRequestVO;
  }

  @Override
  public ActiveProductsRequestVO toActiveProductsRequestVO(
      ActiveProductRequest activeProductRequest) {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    BeanUtils.copyProperties(activeProductRequest, activeProductsRequestVO);
    if(!CollectionUtils.isEmpty(activeProductRequest.getCategoryCodes())){
      activeProductsRequestVO.setCategoryCodes(activeProductRequest.getCategoryCodes().stream()
          .filter(StringUtils::isNotBlank).collect(toList()));
    }
    if(!CollectionUtils.isEmpty(activeProductRequest.getPickupPointCodes())){
      activeProductsRequestVO.setPickupPointCodes(activeProductRequest.getPickupPointCodes().stream()
          .filter(StringUtils::isNotBlank).collect(toList()));
    }

    return activeProductsRequestVO;
  }

  @Override
  public SimpleProductAndItemsMasterDataDetailResponse
  toSimpleProductAndItemsMasterDataDetailResponse(
      SimpleMasterDataDetailWithProductAndItemsResponseVo
          simpleMasterDataDetailWithProductAndItemsResponse){
    return this.gdnMapperHelper.mapBean(simpleMasterDataDetailWithProductAndItemsResponse,
        SimpleProductAndItemsMasterDataDetailResponse.class);
  }

  @Override
  public SimpleProductAndItemsMasterDataDetailV2Response toSimpleProductAndItemsMasterDataDetailV2Response(
      SimpleMasterDataDetailWithProductAndItemsV2ResponseVo simpleMasterDataDetailWithProductAndItemsResponse) {
    return this.gdnMapperHelper.mapBean(simpleMasterDataDetailWithProductAndItemsResponse,
            SimpleProductAndItemsMasterDataDetailV2Response.class);
  }

  @Override
  public List<OfflineItemDetailResponse> toOfflineItemDetailResponse(List<OfflineItemDetailVo> offlineItemDetailVos){
    return offlineItemDetailVos.stream().map(offlineItemDetailVo ->
        this.gdnMapperHelper.mapBean(offlineItemDetailVo, OfflineItemDetailResponse.class)).collect(toList());
  }

  @Override
  public void setPromoTypesForListOfItems(ItemSummaryPageResponseVo itemSummaryPageResponseVo) {
    if (CollectionUtils.isNotEmpty(itemSummaryPageResponseVo.getItemSummaryResponses())) {
      itemSummaryPageResponseVo.getItemSummaryResponses().stream()
          .forEach(itemSummaryResponseVO -> setPromoTypes(itemSummaryResponseVO));
    }
  }

  @Override
  public GdnRestListResponse<ItemSummaryDetailResponse> convertToItemSummaryDetailListResponse(String requestId,
      int page, int size, ItemSummaryPageResponseVo result, String fetchViewConfigByChannel) {
    checkArgument(Objects.nonNull(result), "itemSummaryPageResponseVo must not be null");
    this.setPromoTypesForListOfItems(result);
    ModelConverterImpl.LOG.debug("convertToItemSummaryDetailListResponse ItemSummaryPageResponseVo : {}", result);
    Map<String, ItemSummaryResponseVO> itemSummaryResponseVOMap = new HashMap<>();
    result.getItemSummaryResponses().stream().forEach(itemSummaryResponseVO -> itemSummaryResponseVOMap.put(
        itemSummaryResponseVO.getItemSku() + HYPHEN + itemSummaryResponseVO.getPickupPointCode(),
        itemSummaryResponseVO));
    ItemSummaryDetailPageResponse pageResponses = this.convertToItemSummaryDetailPageResponse(result, fetchViewConfigByChannel);
    pageResponses.getItemSummaryDetailResponses().stream().forEach(
        itemSummaryResponse -> itemSummaryResponse.setVersion(itemSummaryResponseVOMap.get(
            itemSummaryResponse.getItemSku() + HYPHEN + itemSummaryResponse.getPickupPointCode()).getVersion()));
    return new GdnRestListResponse<>(pageResponses.getItemSummaryDetailResponses(),
        new PageMetaData(size, page, pageResponses.getTotalNum()), requestId);
  }

  public ItemSummaryDetailPageResponse convertToItemSummaryDetailPageResponse(ItemSummaryPageResponseVo result,
      String fetchViewConfigByChannel) {
    ItemSummaryDetailPageResponse response = new ItemSummaryDetailPageResponse();
    BeanUtils.copyProperties(result, response, "itemSummaryResponses");
    List<ItemSummaryDetailResponse> itemSummaryDetailResponseList = new ArrayList<>();
    for (ItemSummaryResponseVO itemSummaryResponseVO : result.getItemSummaryResponses()) {
      ItemSummaryDetailResponse itemSummaryDetailResponse = new ItemSummaryDetailResponse();
      BeanUtils.copyProperties(itemSummaryResponseVO, itemSummaryDetailResponse, "salesCatalogs", "itemViewConfigs",
          "masterDataItemImages", "offlinePrices", "price", "b2bFields");
      if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getSalesCatalogs())) {
        itemSummaryDetailResponse.setSalesCatalogs(
            itemSummaryResponseVO.getSalesCatalogs().stream().map(this::convertToSalesCatalogResponse)
                .collect(toList()));
      }
      if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getItemViewConfigs())) {
        if (cncForWarehouseFeatureSwitch) {
          itemSummaryDetailResponse.setItemViewConfigs(
              getItemViewConfigs(fetchViewConfigByChannel, itemSummaryResponseVO.getItemViewConfigs()));
        } else {
          itemSummaryDetailResponse.setItemViewConfigs(
              itemSummaryResponseVO.getItemViewConfigs().stream()
                  .map(this::convertToProductViewConfigResponse).collect(toSet()));
        }
      }
      itemSummaryDetailResponse.setCncActivated(
          setCncActivatedForBackward(itemSummaryDetailResponse.getItemViewConfigs(),
              cncForWarehouseFeatureSwitch, itemSummaryResponseVO.isCncActivated(),
              ItemViewConfigDTO::getChannel, ItemViewConfigDTO::isBuyable));
      if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getItemViewConfigB2b())) {
        itemSummaryDetailResponse.setItemViewConfigB2b(
            itemSummaryResponseVO.getItemViewConfigB2b().stream().map(this::convertToProductViewConfigResponse)
                .collect(toSet()));
      }
      if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getMasterDataItemImages())) {
        itemSummaryDetailResponse.setMasterDataItemImages(
            itemSummaryResponseVO.getMasterDataItemImages().stream().map(this::convertToMasterDataImages)
                .collect(toList()));
      }
      if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getOfflinePrices())) {
        itemSummaryDetailResponse.setOfflinePrices(
            itemSummaryResponseVO.getOfflinePrices().stream().map(this::convertToOfferLinePrices).collect(toList()));
      }
      if (Objects.nonNull(itemSummaryResponseVO.getPrice()) && itemSummaryResponseVO.getPrice()
        .stream().findFirst().isPresent()) {
        itemSummaryDetailResponse.setPrice(Collections.singleton(
          gdnMapperHelper.mapBean(itemSummaryResponseVO.getPrice().stream().findFirst().get(),
            PriceDTO.class)));
      }
      if (Objects.nonNull(itemSummaryResponseVO.getB2bFields())) {
        B2bFieldsDTO b2bFieldsDTO = new B2bFieldsDTO(itemSummaryResponseVO.getB2bFields().isManaged(),
            itemSummaryResponseVO.getB2bFields().getBasePrice());
        itemSummaryDetailResponse.setB2bFields(b2bFieldsDTO);
      }
      itemSummaryDetailResponse.setOriginalPrice(itemSummaryResponseVO.getOriginalSellingPrice());
      itemSummaryDetailResponse
          .setMasterCatalog(convertToMasterCatalogResponse(itemSummaryResponseVO.getMasterCatalog()));
      itemSummaryDetailResponseList.add(itemSummaryDetailResponse);
    }
    response.setItemSummaryDetailResponses(itemSummaryDetailResponseList);
    return response;
  }

  private OfflineItemPriceDTO convertToOfferLinePrices(OfflineItemPriceVO offlineItemPriceVO) {
    checkArgument(Objects.nonNull(offlineItemPriceVO), ModelConverterImpl.OFFLINE_ITEM_LINE_PRICE);
    OfflineItemPriceDTO offlineItemPriceDTO = new OfflineItemPriceDTO();
    offlineItemPriceDTO.setListPrice(offlineItemPriceVO.getListPrice());
    offlineItemPriceDTO.setOfferPrice(offlineItemPriceVO.getOfferPrice());
    offlineItemPriceDTO.setPickupPointCode(offlineItemPriceVO.getPickupPointCode());
    return offlineItemPriceDTO;
  }

  private MasterDataItemImageDTO convertToMasterDataImages(MasterDataItemImage masterDataItemImage) {
    checkArgument(Objects.nonNull(masterDataItemImage), ModelConverterImpl.MASTER_DATA_ITEM_IMAGES);
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setSequence(masterDataItemImage.getSequence());
    masterDataItemImageDTO.setMainImage(masterDataItemImage.isMainImage());
    masterDataItemImageDTO.setLocationPath(masterDataItemImage.getLocationPath());
    return masterDataItemImageDTO;
  }

  private void setPromoTypes(ItemSummaryResponseVO itemSummaryResponseVO) {
    if (CollectionUtils.isNotEmpty(itemSummaryResponseVO.getPrice())) {
      List<String> promoTypes = new ArrayList<>();
      if (itemSummaryResponseVO.isMerchantPromoDiscount()) {
        promoTypes.add(PromoType.PROMO_DISCOUNT.getDescription());
      }
      if (itemSummaryResponseVO.isPromoBundling()) {
        promoTypes.add(PromoType.PROMO_BUNDLING.getDescription());
      }
      Date now = new Date();
      if (itemSummaryResponseVO.getPrice().stream().anyMatch(
          price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices()
              .stream().anyMatch(
                  discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode()) && discountPrice
                      .getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))))) {
        promoTypes.add(PromoType.CAMPAIGN.getDescription());
      }
      itemSummaryResponseVO.setPromoTypes(promoTypes);
    }
  }

  @Override
  public ProductL3Response convertToProductL3Response(ProductAndItemsVO productAndItemsVO) {
    checkArgument(Objects.nonNull(productAndItemsVO), ModelConverterImpl.PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    Product product = productAndItemsVO.getProduct();
    product.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(product.getMasterDataProduct(), false));
    if (product.getMasterDataProduct() != null) {
      product.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
    }
    product
        .setSettlementType(this.productHelperService.getSettlementType(product, productAndItemsVO.getItems().get(0)));
    ProductL3Response productL3Response =
        gdnMapperHelper.mapBean(productAndItemsVO.getProduct(), ProductL3Response.class);
    productL3Response
      .setPickupPointCodes(new ArrayList<>(productAndItemsVO.getProduct().getPickupPointCodes()));
    productL3Response.setFbbPickupPointCodes(Optional.ofNullable(productAndItemsVO.getItemPickupPoints()).map(
        pickUpPoints -> pickUpPoints.stream().filter(ItemPickupPoint::isFbbActivated)
            .map(ItemPickupPoint::getPickupPointCode).distinct().collect(toList())).orElse(new ArrayList<>()));
    productL3Response.getFbbPickupPointCodes()
        .forEach(itemPickupPoint -> productL3Response.getPickupPointCodes().remove(itemPickupPoint));
    productL3Response.setFbbActivated(product.isFbbActivated());
    productL3Response.setMarkForDelete(productAndItemsVO.getProduct().isMarkForDelete());
    if (Objects.nonNull(productAndItemsVO.getProduct().getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productAndItemsVO.getProduct().getProductScore(), productScoreResponse);
      productL3Response.setProductScore(productScoreResponse);
    }
    productL3Response.setPreOrderDTO(checkPreOrder(productAndItemsVO.getProduct(), false));
    if (product.isSynchronized()) {
      productL3Response.setLength(product.getMasterDataProduct().getLength());
      productL3Response.setHeight(product.getMasterDataProduct().getHeight());
      productL3Response.setWidth(product.getMasterDataProduct().getWidth());
      productL3Response.setWeight(product.getMasterDataProduct().getWeight());
      productL3Response.setShippingWeight(product.getMasterDataProduct().getShippingWeight());
    }
    setProductDetailsFromItems(productL3Response, productAndItemsVO.getItems());
    setDisableUnSyncFlagToProductL3Response(productL3Response, productAndItemsVO);
    productL3Response.setFreeSample(product.isFreeSample());
    productL3Response.setProductCenterUpdatedDate(product.getProductCenterUpdatedDate());
    productL3Response.setBundleProduct(product.isBundleProduct());
    productL3Response.setSizeChartCode(product.getSizeChartCode());
    productL3Response.setDistributionMappingStatus(product.getDistributionStatus().getDescription());
    productL3Response.setDimensionsMissing(Optional.ofNullable(product.getDimensionsMissing()).orElseGet(
        () -> Optional.ofNullable(product.getMissingFields()).map(CollectionUtils::isNotEmpty).orElse(null)));
    productL3Response.setVideoUrl(Optional.ofNullable(product.getVideo())
        .map(video -> StringUtils.defaultIfEmpty(video.getFinalUrl(), video.getSourceUrl()))
        .orElse(null));
    productL3Response.setCoverImagePath(
        Optional.ofNullable(product.getVideo()).map(Video::getCoverImagePath).orElse(null));
    productL3Response.setUrl(product.getUrl());
    if (Objects.nonNull(product.getMasterDataProduct())) {
      if (ranchIntegrationEnabled && StringUtils.isNotBlank(product.getMasterDataProduct().getDistributionInfo())) {
        try {
          productL3Response.setDistributionInfoDTO(
              objectMapper.readValue(product.getMasterDataProduct().getDistributionInfo(), DistributionInfoDTO.class));
        } catch (JsonProcessingException e) {
          log.error("Error when trying to read distribution info for productCode : {} ", product.getProductSku());
        }
      }
    }
    if(Objects.nonNull(product.getMasterDataProduct())){
      productL3Response.setAiGeneratedFieldsResponse(product.getMasterDataProduct().getAiGeneratedFieldsResponse());
    }
    return productL3Response;
  }

  @Override
  public List<ItemPickupPointCodeResponse> getItemPickupPointCodeResponseFromItemPickupPointVo(
      List<ItemPickupPointVo> itemPickupPointVos) {
    return itemPickupPointVos.stream().map(
      itemPickupPointVo -> new ItemPickupPointCodeResponse(itemPickupPointVo.getItemSku(),
        itemPickupPointVo.getItemName(), itemPickupPointVo.getPickupPointCode(),
        itemPickupPointVo.getPickupPointName())).collect(toList());
  }

  @Override
  public List<ItemPickupPointVo> toItemPickupPointFromItemVo(Item itemVo,
      List<ItemPickupPointActivationRequest> itemPickupPointActivationRequests, String productSku, boolean online) {
    List<ItemPickupPointVo> itemPickupPointVoList = new ArrayList<>();
    itemVo.setProductSku(productSku);
    int size = Optional.ofNullable(itemPickupPointActivationRequests).orElse(new ArrayList<>()).size();
    boolean delivery = size <= 1;
    for (ItemPickupPointActivationRequest itemPickupPointActivationRequest : Optional.ofNullable(
        itemPickupPointActivationRequests).orElse(new ArrayList<>())) {
      ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
      itemPickupPointVo.setStoreId(Constants.DEFAULT_STORE_ID);
      itemPickupPointVo.setOfflineItemId(new StringBuilder(itemVo.getItemSku()).append(Constants.HYPHEN)
          .append(itemPickupPointActivationRequest.getPickupPointCode()).toString());
      itemPickupPointVo.setProductSku(productSku);
      itemPickupPointVo.setItemSku(itemVo.getItemSku());
      itemPickupPointVo.setPickupPointCode(itemPickupPointActivationRequest.getPickupPointCode());
      itemPickupPointVo.setMerchantSku(itemVo.getMerchantSku());
      itemPickupPointVo.setMerchantCode(itemVo.getMerchantCode());
      itemPickupPointVo.setDelivery(delivery);
      itemPickupPointVo.setCncActive(itemPickupPointActivationRequest.isCncActive());
      itemPickupPointVo.setWholesalePriceExists(itemPickupPointActivationRequest.isWholesalePriceExists());
      itemPickupPointVo.setFbbActivated(itemPickupPointActivationRequest.isFbbActivated());
      itemPickupPointVo.setWholesalePriceActivated(itemPickupPointActivationRequest.isWholesalePriceActivated());
      itemPickupPointVo.setDistribution(itemPickupPointActivationRequest.isDistribution());
      itemPickupPointVo.setPrice(itemPickupPointActivationRequest.getPrice().stream()
          .map(priceRequest -> gdnMapperHelper.mapBean(priceRequest, Price.class)).collect(toSet()));
      itemPickupPointVo.setItemViewConfig(itemPickupPointActivationRequest.getItemViewConfigs().stream()
          .map(itemViewConfigRequest -> gdnMapperHelper.mapBean(itemViewConfigRequest, ItemViewConfig.class))
          .collect(toSet()));
      itemPickupPointVo.setB2bFields(gdnMapperHelper.mapBean(itemPickupPointActivationRequest.getB2bFields(),
          B2bFields.class));
      itemPickupPointVo.setDistribution(itemPickupPointActivationRequest.isDistribution());
      itemPickupPointVoList.add(itemPickupPointVo);
    }
    return itemPickupPointVoList;
  }

  @Override
  public ProductAndItemsResponse convertToProductAndItemsResponse(ProductItemsVo productItemsVo, String fetchViewConfigByChannel, boolean validateViewConfig) {
    checkArgument(Objects.nonNull(productItemsVo), ModelConverterImpl.PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    ProductVo productVo = productItemsVo.getProductVo();
    productVo.getDescriptiveAttributes()
        .addAll(this.fetchDescriptiveAttributes(productVo.getMasterDataProduct(), true));
    Map<String, Map<String, String>> attributeCodeValueAndValueType = new HashMap<>();
    if (Objects.nonNull(productVo.getMasterDataProduct())) {
      productVo.setMasterCatalog(productVo.getMasterDataProduct().getMasterCatalog());
      if (valueTypeAdditionForDefiningAttributes) {
        populateAttributeCodeValueAndValueType(productVo, attributeCodeValueAndValueType);
      }
    }

    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      if (Objects.nonNull(itemVo.getMasterDataItem())) {
        itemVo.setMasterDataItem(
            this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
                productVo.getMasterDataProduct()));
        if (Objects.nonNull(itemVo.getMasterDataItem()) && StringUtils.isNotBlank(
            itemVo.getMasterDataItem().getHash())) {
          itemVo.getMasterDataItem().setHash(StringUtils.SPACE);
        }
      }
    }
    productVo.setSettlementType(
        this.productHelperService.getSettlementType(productVo, productItemsVo.getItemVoList().get(0)));
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    BeanUtils.copyProperties(productItemsVo, productAndItemsResponse, "product", "items");
    ProductResponse productResponse = gdnMapperHelper.mapBean(productItemsVo.getProductVo(), ProductResponse.class);
    productResponse.setMarkForDelete(productResponse.isMarkForDelete());
    if (Objects.nonNull(productVo.getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productVo.getProductScore(), productScoreResponse);
      productResponse.setProductScore(productScoreResponse);
    }
    productResponse.setPreOrder(checkPreOrder(productVo, true));
    productResponse.setTakenDown(productVo.isTakenDown());
    productResponse.setSuspended(productVo.isSuspended());
    productResponse.setHalalProduct(
        CurationStatus.APPROVED.equals(productVo.getCurationStatus()));
    if (resizeImageRemoval && Objects.nonNull(productResponse.getMasterDataProduct())) {
      List<String> pathsToRemove = Arrays.asList(resizeImagePathList.split(Constants.COMMA_DELIMITER));
      productResponse.getMasterDataProduct().getMasterDataProductImages().removeIf(
          masterDataProductImageDTO -> Objects.isNull(masterDataProductImageDTO) || containsAnyPath(
              masterDataProductImageDTO.getLocationPath(), pathsToRemove));
    }
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());
    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      ItemResponse itemResponse = gdnMapperHelper.mapBean(itemVo, ItemResponse.class);
      itemResponse.setCncActive(ResponseHelper.setCncActivatedForBackward(
          itemResponse.getItemViewConfigs(), cncForWarehouseFeatureSwitch,
          itemResponse.isCncActive(), ItemViewConfigDTO::getChannel,
          ItemViewConfigDTO::isBuyable));
      if (validateViewConfig) {
        validateAndUpdateViewConfigs(itemResponse, fetchViewConfigByChannel);
      }
      itemResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(itemVo));
      if(setItemPickUpPointMarkForDeleteValue){
        itemResponse.setItemPickUpPointMarkForDelete(
            Optional.ofNullable(itemVo.getItemPickupPointVoList()).orElse(new ArrayList<>()).stream()
                .filter(Objects::nonNull).findFirst().map(ItemPickupPointVo::isMarkForDelete).orElse(false));
        if (itemResponse.getItemPickUpPointMarkForDelete()) {
          Optional.ofNullable(itemResponse.getItemViewConfigs()).orElse(new HashSet<>())
              .forEach(this::updateItemViewConfigs);
        }
      }
      itemResponse.setMarkForDelete(itemVo.isMarkForDelete());
      itemResponse.setVersion(itemVo.getVersion());
      if (subscriptionAtL5Flow) {
        itemResponse.setSubscribable(itemVo.isSubscribable());
        itemResponse.setSubscribableAtL5Level(
            Optional.ofNullable(itemVo.getItemPickupPointVoList()).orElse(new ArrayList<>()).stream()
                .filter(Objects::nonNull).findFirst().map(ItemPickupPointVo::isSubscribable).orElse(false));
      } else {
        setSubscribableFlagBasedOnPreferredSubscriptionType(itemVo, itemResponse);
      }
      setWholesalePriceActivatedForItems(itemResponse, itemVo);
      if (resizeImageRemoval && Objects.nonNull(itemResponse.getMasterDataItem())) {
        List<String> pathsToRemove = Arrays.asList(resizeImagePathList.split(Constants.COMMA_DELIMITER));
        itemResponse.getMasterDataItem().getMasterDataItemImages().removeIf(
            masterDataItemImageDTO -> Objects.isNull(masterDataItemImageDTO) || containsAnyPath(
                masterDataItemImageDTO.getLocationPath(), pathsToRemove));
      }
      if (valueTypeAdditionForDefiningAttributes) {
        setAttributeValueTypeForItems(attributeCodeValueAndValueType, itemVo, itemResponse);
      }
      productAndItemsResponse.getItems().add(itemResponse);
    }
    setDocumentType(productAndItemsResponse);
    productAndItemsResponse =
        setDisableUnSyncFlagToProductAndItemsResponse(productVo.getStoreId(), productAndItemsResponse);
    ResponseHelper.setMainImageForProducts(productAndItemsResponse.getProduct(), productAndItemsResponse.getItems(),
        setMainImageIfNotExists);
    allowedAttributeValuesService.sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(),
        productAndItemsResponse);
    if (setDefaultProductType && Objects.isNull(productAndItemsResponse.getProduct().getProductType())) {
      productAndItemsResponse.getProduct().setProductType(ProductType.REGULAR);
    }
    ProductType productType = productAndItemsResponse.getProduct().getProductType();
    productAndItemsResponse.getItems().forEach(itemResponse -> itemResponse.setLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(productType, overrideLateFulfillmentByProductType,
            itemResponse.getLateFulfillment())));
    Optional.ofNullable(productAndItemsResponse.getProduct()).stream()
      .map(ProductResponse::getMasterDataProduct).filter(Objects::nonNull).forEach(
        masterDataProductDTO -> masterDataProductDTO.setSizeAttributeValueType(
          getSizeAttributeType(masterDataProductDTO)));
    ResponseHelper.setVideoUrlAndYoutubeUrl(productAndItemsResponse, productVo);
    try {
      ResponseHelper.removeDuplicateProductAttributeValue(productAndItemsResponse, removeDuplicateProductAttribute);
    }
    catch (Exception e) {
      log.error("Error while removing duplicate product attribute and values ", e);
    }
    return productAndItemsResponse;
  }

  public void validateAndUpdateViewConfigs(ItemResponse itemResponse,
      String fetchViewConfigByChannel) {
    if(!cncForWarehouseFeatureSwitch) {
      Set<String> channels = Collections.singleton(Constants.DEFAULT_CHANNEL);
      itemResponse.setItemViewConfigs(filterViewConfig(channels, itemResponse.getItemViewConfigs()));
    } else {
      Pair<Set<String>, Boolean> allowedConfigSetAndFilterNeededPair =
          ResponseHelper.getRequestViewConfigSet(fetchViewConfigByChannel, cncForWarehouseFeatureSwitch);
      if (allowedConfigSetAndFilterNeededPair.getSecond()) {
        itemResponse.setItemViewConfigs(filterViewConfig(allowedConfigSetAndFilterNeededPair.getFirst(), itemResponse.getItemViewConfigs()));
      }
    }
  }

  public void validateAndUpdateViewConfigs(BasicItemDTO basicItemDTO,
      String fetchViewConfigByChannel) {
    if(!cncForWarehouseFeatureSwitch) {
      Set<String> channels = Collections.singleton(Constants.DEFAULT_CHANNEL);
      basicItemDTO.setItemViewConfigs(filterViewConfig(channels, basicItemDTO.getItemViewConfigs()));
    } else {
      Pair<Set<String>, Boolean> allowedConfigSetAndFilterNeededPair =
          ResponseHelper.getRequestViewConfigSet(fetchViewConfigByChannel, cncForWarehouseFeatureSwitch);
      if (allowedConfigSetAndFilterNeededPair.getSecond()) {
        basicItemDTO.setItemViewConfigs(filterViewConfig(allowedConfigSetAndFilterNeededPair.getFirst(), basicItemDTO.getItemViewConfigs()));
      }
    }
  }

  private Set<ItemViewConfigDTO> filterViewConfig(Set<String> channels, Collection<ItemViewConfigDTO> allViewConfigs){
    return allViewConfigs.stream()
        .filter(config -> channels.contains(config.getChannel()))
        .collect(Collectors.toSet());
  }

  private void populateSingleAttributeValue(MasterDataProductAttributeValue masterDataProductAttributeValue,
      Map<String, Map<String, String>> attributeCodeValueAndValueType) {
    if (Objects.nonNull(masterDataProductAttributeValue.getAllowedAttributeValue()) && StringUtils.isNotBlank(
        masterDataProductAttributeValue.getAllowedAttributeValue().getAllowedAttributeValueCode())) {
      String attributeCode = masterDataProductAttributeValue.getAllowedAttributeValue().getAllowedAttributeValueCode();
      String attributeValue = masterDataProductAttributeValue.getAllowedAttributeValue().getValue();
      String valueType = masterDataProductAttributeValue.getAllowedAttributeValue().getValueType();
      Map<String, String> valueAndTypeMap = new HashMap<>();
      valueAndTypeMap.put(attributeValue, valueType);
      attributeCodeValueAndValueType.put(attributeCode, valueAndTypeMap);
    }
  }

  @Override
  public void populateAttributeCodeValueAndValueType(Product product,
      Map<String, Map<String, String>> attributeCodeValueAndValueType) {
    if (Objects.nonNull(product.getMasterDataProduct())) {
      product.getMasterDataProduct().getMasterDataProductAttributes().forEach(
          masterDataProductAttribute -> masterDataProductAttribute.getMasterDataProductAttributeValues().forEach(
              masterDataProductAttributeValue -> populateSingleAttributeValue(masterDataProductAttributeValue,
                  attributeCodeValueAndValueType)));
    }
  }

  private String getSizeAttributeType(MasterDataProductDTO masterDataProduct) {
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOS = new ArrayList<>();
    Set<String> valueTypes = new HashSet<>();
    if (Objects.nonNull(masterDataProduct)) {
      masterDataProductAttributeDTOS =
          masterDataProduct.getMasterDataProductAttributes().stream().filter(Objects::nonNull).filter(
                  masterDataProductAttributeDTO -> masterDataProductAttributeDTO.getMasterDataAttribute().isSizeAttribute())
              .collect(toList());
    }
    masterDataProductAttributeDTOS.forEach(
        masterDataProductAttributeDTO -> masterDataProductAttributeDTO.getMasterDataProductAttributeValues()
            .forEach(masterDataProductAttributeValueDTO -> {
              if (Objects.nonNull(masterDataProductAttributeValueDTO.getAllowedAttributeValue())) {
                String valueType = masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValueType();
                if (Objects.nonNull(valueType)) {
                  valueTypes.add(valueType);
                }
              }
            }));
    return valueTypes.size() > Constants.ONE ? valueTypes.stream().findFirst().get() : null;
  }

  public void setSizeAttributeValueType(ProductAndItemsResponse productAndItemsResponse) {
    if (valueTypeAdditionForDefiningAttributes && Objects.nonNull(
        productAndItemsResponse.getProduct().getMasterDataProduct())) {
      productAndItemsResponse.getProduct().getMasterDataProduct()
          .setSizeAttributeValueType(getSizeAttributeType(productAndItemsResponse.getProduct().getMasterDataProduct()));
    }
  }

  private void populateAttributeCodeValueAndValueType(ProductVo productVo,
      Map<String, Map<String, String>> attributeCodeValueAndValueType) {
    if (Objects.nonNull(productVo.getMasterDataProduct())) {
      productVo.getMasterDataProduct().getMasterDataProductAttributes().forEach(
          masterDataProductAttribute -> masterDataProductAttribute.getMasterDataProductAttributeValues().forEach(
              masterDataProductAttributeValue -> populateSingleAttributeValue(masterDataProductAttributeValue,
                  attributeCodeValueAndValueType)));
    }
  }

  private Optional<String> getValueTypeForAttribute(String attributeCode, String attributeValue,
      Map<String, Map<String, String>> attributeCodeValueAndValueType) {
    return attributeCodeValueAndValueType.entrySet().stream().filter(entry -> entry.getKey().contains(attributeCode))
        .map(Map.Entry::getValue).filter(valueValueTypeMap -> valueValueTypeMap.containsKey(attributeValue))
        .map(valueValueTypeMap -> valueValueTypeMap.get(attributeValue)).filter(Objects::nonNull).findFirst();
  }

  private void processAndSetValueType(MasterDataItemAttributeValueDTO masterDataItemAttributeValue,
      Map<String, Map<String, String>> attributeCodeValueAndValueType) {
    Optional.ofNullable(masterDataItemAttributeValue).map(MasterDataItemAttributeValueDTO::getMasterDataAttribute)
        .map(MasterDataAttributeDTO::getAttributeCode).flatMap(
            attributeCode -> getValueTypeForAttribute(attributeCode, masterDataItemAttributeValue.getAttributeValue(),
                attributeCodeValueAndValueType)).ifPresent(
            masterDataItemAttributeValue != null ? masterDataItemAttributeValue::setValueType : null);
  }

  private void setAttributeValueTypeForItems(Map<String, Map<String, String>> attributeCodeValueAndValueType,
      ItemVo itemVo, ItemResponse itemResponse) {
    if (Objects.nonNull(itemVo.getMasterDataItem())) {
      itemResponse.getMasterDataItem().getMasterDataItemAttributeValues().forEach(
          masterDataItemAttributeValue -> processAndSetValueType(masterDataItemAttributeValue,
              attributeCodeValueAndValueType));
    }
  }

  private void updateItemViewConfigs(ItemViewConfigDTO itemViewConfigDTO){
    itemViewConfigDTO.setBuyable(false);
    itemViewConfigDTO.setDiscoverable(false);
    Optional.ofNullable(itemViewConfigDTO.getItemBuyableSchedules()).orElse(new ItemBuyableScheduleDTO())
        .setBuyable(false);
    Optional.ofNullable(itemViewConfigDTO.getItemDiscoverableSchedules()).orElse(new ItemDiscoverableScheduleDTO())
        .setDiscoverable(false);
  }

  private boolean containsAnyPath(String locationPath, List<String> pathsToRemove) {
    locationPath = Optional.ofNullable(locationPath).orElse(StringUtils.EMPTY);
    return pathsToRemove.stream().anyMatch(locationPath::contains);
  }

  private void setSubscribableFlagBasedOnPreferredSubscriptionType(ItemVo itemVo, ItemResponse itemResponse) {
    if (itemVo.isSubscribable()) {
      if (CollectionUtils.isEmpty(itemVo.getPreferredSubscriptionType()) || Stream.of(WAREHOUSE, MARKETPLACE)
          .allMatch(itemVo.getPreferredSubscriptionType()::contains)) {
        itemResponse.setSubscribable(true);
      } else if (itemVo.getPreferredSubscriptionType().size() == 1 && itemVo.getPreferredSubscriptionType()
          .contains(WAREHOUSE)) {
        itemResponse.setSubscribable(itemVo.getItemPickupPointVoList().get(0).isFbbActivated());
      } else if (itemVo.getPreferredSubscriptionType().size() == 1 && itemVo.getPreferredSubscriptionType()
          .contains(MARKETPLACE)) {
        itemResponse.setSubscribable(!itemVo.getItemPickupPointVoList().get(0).isFbbActivated());
      }
    } else {
      itemResponse.setSubscribable(false);
    }
  }

  @Override
  public ProductAndItemsResponse convertToProductAndItemsResponseWithConvertPreOrderDetails(
      ProductItemsVo productItemsVo, boolean convertPreOrderDetails,
      String fetchViewConfigByChannel, boolean validateViewConfig) {
    checkArgument(Objects.nonNull(productItemsVo), ModelConverterImpl.PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    ProductVo productVo = productItemsVo.getProductVo();
    productVo.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(productVo.getMasterDataProduct(), false));
    if (Objects.nonNull(productVo.getMasterDataProduct())) {
      productVo.setMasterCatalog(productVo.getMasterDataProduct().getMasterCatalog());
    }

    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      if (Objects.nonNull(itemVo.getMasterDataItem())) {
        itemVo.setMasterDataItem(
            this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
                productVo.getMasterDataProduct()));
        if (Objects.nonNull(itemVo.getMasterDataItem()) && StringUtils.isNotBlank(
            itemVo.getMasterDataItem().getHash())) {
          itemVo.getMasterDataItem().setHash(StringUtils.SPACE);
        }
      }
    }
    productVo.setSettlementType(
        this.productHelperService.getSettlementType(productVo, productItemsVo.getItemVoList().get(0)));
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    BeanUtils.copyProperties(productItemsVo, productAndItemsResponse, "product", "items");
    ProductResponse productResponse = gdnMapperHelper.mapBean(productItemsVo.getProductVo(), ProductResponse.class);
    productResponse.setMarkForDelete(productResponse.isMarkForDelete());
    productResponse.setHalalProduct(
        CurationStatus.APPROVED.equals(productVo.getCurationStatus()));
    if (Objects.nonNull(productVo.getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productVo.getProductScore(), productScoreResponse);
      productResponse.setProductScore(productScoreResponse);
    }
    productResponse.setPreOrder(checkPreOrderProductVo(productVo, convertPreOrderDetails));
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());
    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      ItemResponse itemResponse = gdnMapperHelper.mapBean(itemVo, ItemResponse.class);
      itemResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(itemVo));
      itemResponse.setMarkForDelete(itemVo.isMarkForDelete());
      itemResponse.setVersion(itemVo.getVersion());
      setWholesalePriceActivatedForItems(itemResponse, itemVo);
      itemResponse.setCncActive(ResponseHelper.setCncActivatedForBackward(
          itemResponse.getItemViewConfigs(), cncForWarehouseFeatureSwitch,
          itemResponse.isCncActive(), ItemViewConfigDTO::getChannel,
          ItemViewConfigDTO::isBuyable));
      if (CollectionUtils.isEmpty(itemVo.getItemViewConfigs())) {
        ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
        itemViewConfigDTO.setChannel(Constants.DEFAULT_CHANNEL);
        itemResponse.setItemViewConfigs(ImmutableSet.of(itemViewConfigDTO));
      } else if(validateViewConfig) {
        validateAndUpdateViewConfigs(itemResponse, fetchViewConfigByChannel);
      }
      productAndItemsResponse.getItems().add(itemResponse);
    }
    setDocumentType(productAndItemsResponse);
    if (StringUtils.isNotBlank(imeiAttributeCode)) {
      setImeiRequiredFlagAtL3(productVo, productAndItemsResponse);
    }
    return setDisableUnSyncFlagToProductAndItemsResponse(productVo.getStoreId(), productAndItemsResponse);
  }

  private void setImeiRequiredFlagAtL3(Product product,
    ProductAndItemsResponse productAndItemsResponse) {
    Optional.ofNullable(product.getProductSpecialAttributes()).filter(CollectionUtils::isNotEmpty)
      .map(specialAttributes -> specialAttributes.stream().filter(Objects::nonNull)
        .filter(specialAttribute -> StringUtils.equals(specialAttribute.getAttributeCode(),
          imeiAttributeCode))
        .collect(Collectors.toMap(ProductSpecialAttribute::getAttributeCode,
          ProductSpecialAttribute::getAttributeValue, (old, newValue) -> newValue)))
      .ifPresent(attributeMap -> {
        String imeiValue = attributeMap.getOrDefault(imeiAttributeCode, StringUtils.EMPTY);
        productAndItemsResponse.getProduct().setImeiRequired(imeiAllowedValues.contains(imeiValue));
      });
  }

  @Override
  public ProductAndItemDataResponse convertToProductAndItemDataResponse(ProductItemsVo productItemsVo) {
    checkArgument(Objects.nonNull(productItemsVo),
        ModelConverterImpl.PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    ProductVo productVo = productItemsVo.getProductVo();
    productVo.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(productVo.getMasterDataProduct(), false));
    if (Objects.nonNull(productVo.getMasterDataProduct())) {
      productVo.setMasterCatalog(productVo.getMasterDataProduct().getMasterCatalog());
    }
    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      if (Objects.nonNull(itemVo.getMasterDataItem())) {
        itemVo.setMasterDataItem(this.masterDataConstructorService
            .constructItemDimensionFields(itemVo.getMasterDataItem(), productVo.getMasterDataProduct()));
        if (Objects.nonNull(itemVo.getMasterDataItem()) && StringUtils.isNotBlank(itemVo.getMasterDataItem().getHash())) {
          itemVo.getMasterDataItem().setHash(StringUtils.SPACE);
        }
      }
    }
    productVo.setSettlementType(this.productHelperService.getSettlementType(productVo,
        productItemsVo.getItemVoList().get(0)));
    ProductAndItemDataResponse productAndItemsResponse = new ProductAndItemDataResponse();
    BeanUtils.copyProperties(productItemsVo, productAndItemsResponse, "product", "items");
    ProductResponse productResponse =
        gdnMapperHelper.mapBean(productItemsVo.getProductVo(), ProductResponse.class);
    productResponse.setMarkForDelete(productResponse.isMarkForDelete());
    if (Objects.nonNull(productVo.getProductScore())) {
      ProductScoreResponse productScoreResponse = new ProductScoreResponse();
      BeanUtils.copyProperties(productVo.getProductScore(), productScoreResponse);
      productResponse.setProductScore(productScoreResponse);
    }
    productResponse.setPreOrder(checkPreOrderProductVo(productVo, true));
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(new ArrayList<>());
    for (ItemVo itemVo: productItemsVo.getItemVoList()) {
      ItemDataResponse itemResponse = gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class);
      itemResponse.setMarkForDelete(itemVo.isMarkForDelete());
      itemResponse.setVersion(itemVo.getVersion());
      productAndItemsResponse.getItems().add(itemResponse);
    }
    setDocumentType(productResponse);
    return productAndItemsResponse;
  }

  private void setProductDetailsFromItems(ProductL3Response productL3Response, List<Item> items) {
    int count = 0;
    List<String> itemSkus = new ArrayList<>();
    Map<String, String> itemSkuItemCodeMap = new HashMap<>();
    for (Item item : items) {
      if (count == 0) {
        productL3Response.setDefaultItemSku(item.getItemSku());
        productL3Response.setIsLateFulfillment(item.isLateFulfillment());
        productL3Response.setDangerousLevel(item.getDangerousLevel());
        if (!item.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
          productL3Response.setLength(item.getMasterDataItem().getItemLength());
          productL3Response.setHeight(item.getMasterDataItem().getItemHeight());
          productL3Response.setWidth(item.getMasterDataItem().getItemWidth());
          productL3Response.setWeight(item.getMasterDataItem().getItemWeight());
          productL3Response.setShippingWeight(item.getMasterDataItem().getItemDeliveryWeight());
        }
      }
      if (!item.isMarkForDelete()) {
        count++;
      }
      if (!item.isPermanentDelete()) {
        itemSkus.add(item.getItemSku());
        itemSkuItemCodeMap.put(item.getItemSku(), item.getItemCode());
      }
      if (addDeleteVariantSwitch && !item.isMarkForDelete()) {
        productL3Response.setDefaultItemSku(item.getItemSku());
      }
    }
    productL3Response.setItemCount(count);
    productL3Response.setItemSkus(itemSkus);
    productL3Response.setItemSkuItemCodeMap(itemSkuItemCodeMap);
  }

  private void setDisableUnSyncFlagToProductL3Response(ProductL3Response productL3Response,
      ProductAndItemsVO productAndItemsVO) {
    SystemParameter categoryCodeSystemParameter;
    try {
      categoryCodeSystemParameter = this.systemParameterService
          .findValueByStoreIdAndVariable(productAndItemsVO.getProduct().getStoreId(), Constants.CATEGORY_CODE_VARIABLE);
    } catch (ApplicationRuntimeException e) {
      return;
    }
    List<String> categoryCodesForDisableUnSyncList = Arrays.asList(
        Optional.ofNullable(categoryCodeSystemParameter.getValue()).orElse(StringUtils.EMPTY)
            .split(Constants.COMMA_DELIMITER));

    if (Objects.nonNull(productL3Response.getMasterCatalog())) {
      if (Objects.nonNull(productL3Response.getMasterCatalog().getCategory())) {
        if (CollectionUtils.isNotEmpty(categoryCodesForDisableUnSyncList) && categoryCodesForDisableUnSyncList
            .contains(productL3Response.getMasterCatalog().getCategory().getCategoryCode())) {
          productL3Response.setDisableUnSync(true);
        }
      }
    }
  }

  private PreOrderDTO checkPreOrder(Product product, boolean convertPreOrderDetails) {
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    if (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE.equals(product.getPreOrder().getIsPreOrder())) {
      BeanUtils.copyProperties(product.getPreOrder(), preOrderDTO);
      if (Constants.WEEK.equals(product.getPreOrder().getPreOrderType())) {
        if (convertPreOrderDetails) {
          preOrderDTO.setPreOrderType(Constants.DAYS);
          preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue() * Constants.NUM_OF_DAYS_IN_WEEK);
        } else {
          preOrderDTO.setPreOrderType(Constants.WEEK);
          preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
        }
      } else if (Constants.DAYS.equals(product.getPreOrder().getPreOrderType())) {
        preOrderDTO.setPreOrderType(Constants.DAYS);
        preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
      } else if (convertPreOrderDetails && checkPreOrderDateStatus(product.getPreOrder().getPreOrderDate())) {
        preOrderDTO = PreOrderDTO.builder().isPreOrder(false).build();
      }
    } else {
      preOrderDTO.setIsPreOrder(false);
    }
    return preOrderDTO;
  }

  private PreOrderDTO checkPreOrderProductVo(ProductVo product, boolean convertPreOrderDetails) {
    PreOrderDTO preOrderDTO = new PreOrderDTO();
    if (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE.equals(product.getPreOrder().getIsPreOrder())) {
      BeanUtils.copyProperties(product.getPreOrder(), preOrderDTO);
      if (Constants.WEEK.equals(product.getPreOrder().getPreOrderType())) {
        if (convertPreOrderDetails) {
          preOrderDTO.setPreOrderType(Constants.DAYS);
          preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue() * Constants.NUM_OF_DAYS_IN_WEEK);
        } else {
          preOrderDTO.setPreOrderType(Constants.WEEK);
          preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
        }
      } else if (Constants.DAYS.equals(product.getPreOrder().getPreOrderType())) {
        preOrderDTO.setPreOrderType(Constants.DAYS);
        preOrderDTO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
      } else if (convertPreOrderDetails && checkPreOrderDateStatus(product.getPreOrder().getPreOrderDate())) {
        preOrderDTO = PreOrderDTO.builder().isPreOrder(false).build();
      }
    } else {
      preOrderDTO.setIsPreOrder(false);
    }
    return preOrderDTO;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponse convertToMasterDataWithProductItemsVo(
    MasterDataWithProductItemsVo masterDataWithProductItemsVo) {
    for (ProductItemsVo productItemsVo : masterDataWithProductItemsVo.getProductItemsVos()) {
      ProductVo productVo = productItemsVo.getProductVo();
      productVo.setMasterDataProduct(productVo.getMasterDataProduct());
      MasterDataProduct masterDataProduct;
      if (productVo.isSynchronized()) {
        masterDataProduct =
          masterDataWithProductItemsVo.getMasterDataProducts().get(productVo.getProductCode());
      } else {
        masterDataProduct = productVo.getMasterDataProduct();
      }
      productVo.getDescriptiveAttributes()
        .addAll(this.fetchDescriptiveAttributes(masterDataProduct, false));
      for (ItemVo itemVo : productItemsVo.getItemVoList()) {
        if (!itemVo.isSynchronized()) {
          itemVo.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(
            itemVo.getMasterDataItem(), productVo.getMasterDataProduct()));
        }
      }
      productVo.setSettlementType(this.productHelperService.getSettlementType(productVo,
        productItemsVo.getItemVoList().get(0)));
    }
    for (Entry<String, MasterDataItem> entry : masterDataWithProductItemsVo.getMasterDataItems()
      .entrySet()) {
      this.masterDataConstructorService.constructItemDimensionFields(entry.getValue(),
        masterDataWithProductItemsVo.getMasterDataProducts()
          .get(entry.getValue().getProductCode()));
    }
    MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse =
      this.gdnMapperHelper.mapBean(masterDataWithProductItemsVo,
        MasterDataDetailWithProductAndItemsResponse.class);
    masterDataDetailWithProductAndItemsResponse.setProductAndItems(
      masterDataWithProductItemsVo.getProductItemsVos().stream()
        .map(productItemsVo -> toProductItemsVo(productItemsVo)).collect(toList()));
    setDocumentType(masterDataDetailWithProductAndItemsResponse);
    overrideLateFulfillmentInMasterDataProductAndItemsResponse(masterDataDetailWithProductAndItemsResponse);
    return masterDataDetailWithProductAndItemsResponse;
  }

  @Override
  public List<ItemPickupPointListingUpdateRequestVo> convertToItemPickupPointListingUpdateRequestVo(
    ItemPickupPointListingUpdateRequest itemPickupPointListingUpdateRequest) {
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = new ArrayList<>();
    for (ItemPickupPointQuickEditRequest quickEditUpdateRequest : itemPickupPointListingUpdateRequest
      .getQuickEditUpdateRequests()) {
      ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo =
          getItemPickupPointListingUpdateRequestVo(quickEditUpdateRequest);
      itemListingUpdateRequestVos.add(itemListingUpdateRequestVo);
    }
    return itemListingUpdateRequestVos;
  }

  private ItemPickupPointListingUpdateRequestVo getItemPickupPointListingUpdateRequestVo(
      ItemPickupPointQuickEditRequest quickEditUpdateRequest) {
    ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo = new ItemPickupPointListingUpdateRequestVo();
    itemListingUpdateRequestVo.setItemSku(quickEditUpdateRequest.getItemSku());
    itemListingUpdateRequestVo.setDistribution(quickEditUpdateRequest.getDistribution());
    itemListingUpdateRequestVo.setMerchantSku(quickEditUpdateRequest.getMerchantSku());
    if (Objects.nonNull(quickEditUpdateRequest.getOff2OnActiveFlag())) {
      itemListingUpdateRequestVo.setOff2OnChannelActive(quickEditUpdateRequest.getOff2OnActiveFlag());
    }
    itemListingUpdateRequestVo.setPickupPointCode(quickEditUpdateRequest.getPickupPointCode());
    itemListingUpdateRequestVo.setWholesalePriceActivated(quickEditUpdateRequest.getWholeSaleActivated());
    itemListingUpdateRequestVo.setPrice(getPriceFromPriceDto(quickEditUpdateRequest.getPrice()));
    itemListingUpdateRequestVo.setItemViewConfigs(new HashSet<>());
    itemListingUpdateRequestVo.getItemViewConfigs().addAll(
        getItemViewConfigFromStatus(quickEditUpdateRequest.getStatus(), channelService.getDefaultChannel()));
    if (StringUtils.isNotBlank(quickEditUpdateRequest.getCncStatus())) {
      itemListingUpdateRequestVo.getItemViewConfigs()
          .addAll(getItemViewConfigFromStatus(quickEditUpdateRequest.getCncStatus(), channelService.getCncChannel()));
    }
    itemListingUpdateRequestVo.setCncActivated(quickEditUpdateRequest.isCncActivated());
    itemListingUpdateRequestVo.setFbbActivated(quickEditUpdateRequest.isFbbActivated());
    itemListingUpdateRequestVo.setMerchantCode(quickEditUpdateRequest.getMerchantCode());
    itemListingUpdateRequestVo
      .setPpCodeChangedForNonMppSeller(quickEditUpdateRequest.isPpCodeChangedForNonMppSeller());
    if (Objects.nonNull(quickEditUpdateRequest.getB2bFields())) {
      B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
      b2bFieldsVo.setManaged(quickEditUpdateRequest.getB2bFields().isManaged());
      b2bFieldsVo.setBasePrice(quickEditUpdateRequest.getB2bFields().getBasePrice());
      Set<ItemViewConfig> b2bItemViewConfigs =
          getItemViewConfigFromStatus(quickEditUpdateRequest.getB2bFields().getStatus(),
              channelService.getB2BChannel());
      b2bFieldsVo.setB2bItemViewConfigs(b2bItemViewConfigs);
      itemListingUpdateRequestVo.setB2bFieldsVo(b2bFieldsVo);
    }
    setSchedulesForUpdateAtL5(quickEditUpdateRequest, itemListingUpdateRequestVo);
    return itemListingUpdateRequestVo;
  }

  private static void setSchedulesForUpdateAtL5(
    ItemPickupPointQuickEditRequest quickEditUpdateRequest,
    ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo) {
    itemListingUpdateRequestVo.setScheduleRemoval(quickEditUpdateRequest.isScheduleUpdate());

    Optional.ofNullable(quickEditUpdateRequest.getBuyableSchedule())
      .ifPresent(buyableScheduleRequest -> {
        ItemBuyableSchedule buyableSchedule = new ItemBuyableSchedule();
        buyableSchedule.setStartDateTime(buyableScheduleRequest.getStartDateTime());
        buyableSchedule.setEndDateTime(buyableScheduleRequest.getEndDateTime());
        buyableSchedule.setBuyable(buyableScheduleRequest.isBuyable());
        BuyableScheduleVo buyableScheduleVo = new BuyableScheduleVo();
        BeanUtils.copyProperties(buyableSchedule, buyableScheduleVo);
        buyableSchedule.setBuyable(true);
        itemListingUpdateRequestVo.setBuyableScheduleVo(buyableScheduleVo);
      });

    Optional.ofNullable(quickEditUpdateRequest.getDiscoverableSchedule())
      .ifPresent(discoverableScheduleRequest -> {
        ItemDiscoverableSchedule discoverableSchedule = new ItemDiscoverableSchedule();
        discoverableSchedule.setDiscoverable(discoverableScheduleRequest.isDiscoverable());
        discoverableSchedule.setStartDateTime(discoverableScheduleRequest.getStartDateTime());
        discoverableSchedule.setEndDateTime(discoverableScheduleRequest.getEndDateTime());
        DiscoverableScheduleVo discoverableScheduleVo = new DiscoverableScheduleVo();
        BeanUtils.copyProperties(discoverableSchedule, discoverableScheduleVo);
        discoverableScheduleVo.setDiscoverable(true);
        itemListingUpdateRequestVo.setDiscoverableScheduleVo(discoverableScheduleVo);
      });
  }

  @Override
  public ItemPickupPointUpdateRequestVo covertToItemPickupPointUpdateRequestVo(
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setOnline(itemPickupPointUpdateRequest.getOnline());
    itemPickupPointUpdateRequestVo.setCnc(itemPickupPointUpdateRequest.getCnc());
    itemPickupPointUpdateRequestVo.setFbbActivated(itemPickupPointUpdateRequest.getFbbActivated());
    itemPickupPointUpdateRequestVo.setB2bActivated(itemPickupPointUpdateRequest.getB2bActivated());
    itemPickupPointUpdateRequestVo.setB2cActivated(itemPickupPointUpdateRequest.getB2cActivated());
    itemPickupPointUpdateRequestVo.setProductType(itemPickupPointUpdateRequest.getProductType());
    itemPickupPointUpdateRequestVo.setProductSku(itemPickupPointUpdateRequest.getProductSku());
    if (Objects.nonNull(itemPickupPointUpdateRequest.getAddDeleteVariantRequest())) {
      itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(
          getAddDeleteVariantRequestVo(itemPickupPointUpdateRequest.getAddDeleteVariantRequest()));
    }
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(
        Optional.ofNullable(itemPickupPointUpdateRequest.getQuickEditUpdateRequests()).orElseGet(Collections::emptyList)
            .stream().map(this::getItemPickupPointListingUpdateRequestVo).collect(toList()));
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Optional.ofNullable(itemPickupPointUpdateRequest.getAddPickupPointRequests()).orElseGet(Collections::emptyList)
            .stream().map(this::getItemPickupPointListingUpdateRequestVo).collect(toList()));
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(getItemPickupPointDeleteRequestVos(itemPickupPointUpdateRequest));
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequest.getBundleRecipesRequests())) {
      itemPickupPointUpdateRequestVo.setBundleRecipeRequests(itemPickupPointUpdateRequest.getBundleRecipesRequests());
    }
    return itemPickupPointUpdateRequestVo;
  }

  private AddDeleteVariantRequestVo getAddDeleteVariantRequestVo(AddDeleteVariantRequest addDeleteVariantRequest) {
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    addDeleteVariantRequestVo.setAddVariantsList(
        addDeleteVariantRequest.getAddVariantsList().stream().map(this::getAddVariantRequestVo).collect(toList()));
    addDeleteVariantRequestVo.setDeleteVariantsList(addDeleteVariantRequest.getDeleteVariantsList());
    return addDeleteVariantRequestVo;
  }

  private AddVariantRequestVo getAddVariantRequestVo(AddVariantRequest addVariantRequest) {
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    BeanUtils.copyProperties(addVariantRequest, addVariantRequestVo, "itemPickupPoints", "definingAttributes");
    addVariantRequestVo.setItemPickupPoints(
        Optional.ofNullable(addVariantRequest.getItemPickupPoints()).orElseGet(Collections::emptyList).stream()
            .map(this::getItemPickupPointListingUpdateRequestVo).collect(toList()));
    addVariantRequestVo.setDefiningAttributes(getProductAttributeDetailVoList(
        Optional.ofNullable(addVariantRequest.getDefiningAttributes()).orElseGet(Collections::emptyList)));
    return addVariantRequestVo;
  }

  private List<ProductAttributeDetailVo> getProductAttributeDetailVoList(
      List<ProductAttributeDetailDTO> productAttributeDetailDTOList) {
    return productAttributeDetailDTOList.stream().map(this::getProductAttributeDetailVo).collect(toList());
  }

  private ProductAttributeDetailVo getProductAttributeDetailVo(ProductAttributeDetailDTO productAttributeDetailDTO) {
    ProductAttributeDetailVo productAttributeDetailVo = new ProductAttributeDetailVo();
    BeanUtils.copyProperties(productAttributeDetailDTO, productAttributeDetailVo);
    return productAttributeDetailVo;
  }
  private List<ItemPickupPointDeleteRequestVo> getItemPickupPointDeleteRequestVos(
    ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    List<ItemPickupPointDeleteRequestVo> itemPickupPointDeleteRequestVos = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequest.getDeletePickupPointRequests())) {
      for (ItemPickupPointDeleteRequest itemPickupPointDeleteRequest : itemPickupPointUpdateRequest
        .getDeletePickupPointRequests()) {
        ItemPickupPointDeleteRequestVo itemPickupPointDeleteRequestVo = new ItemPickupPointDeleteRequestVo();
        itemPickupPointDeleteRequestVo.setItemSku(itemPickupPointDeleteRequest.getItemSku());
        itemPickupPointDeleteRequestVo.setPickupPointCode(itemPickupPointDeleteRequest.getPickupPointCode());
        itemPickupPointDeleteRequestVos.add(itemPickupPointDeleteRequestVo);
      }
    }
    return itemPickupPointDeleteRequestVos;
  }

  private List<ItemPickupPointDeleteRequestVo> getItemPickupPointListingUpdateRequestVo(
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    List<ItemPickupPointDeleteRequestVo> itemPickupPointDeleteRequestVos = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequest.getDeletePickupPointRequests())) {
      for (ItemPickupPointDeleteRequest itemPickupPointDeleteRequest : itemPickupPointUpdateRequest
          .getDeletePickupPointRequests()) {
        ItemPickupPointDeleteRequestVo itemPickupPointDeleteRequestVo = new ItemPickupPointDeleteRequestVo();
        itemPickupPointDeleteRequestVo.setItemSku(itemPickupPointDeleteRequest.getItemSku());
        itemPickupPointDeleteRequestVo.setPickupPointCode(itemPickupPointDeleteRequest.getPickupPointCode());
        itemPickupPointDeleteRequestVos.add(itemPickupPointDeleteRequestVo);
      }
        new ItemPickupPointListingUpdateRequestVo();
    }
    return itemPickupPointDeleteRequestVos;
  }

  @Override
  public ProductAndItemInfoResponseV2 convertToProductAndItemInfoResponseV2(ProductItemsVo productItemsVo) {
    checkArgument(Objects.nonNull(productItemsVo), PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    ProductVo productVo = productItemsVo.getProductVo();
    productVo.getDescriptiveAttributes().addAll(this.fetchDescriptiveAttributes(productVo.getMasterDataProduct(), false));
    if (Objects.nonNull(productVo.getMasterDataProduct())) {
      productVo.setMasterCatalog(productVo.getMasterDataProduct().getMasterCatalog());
    }
    ItemVo itemVo = productItemsVo.getItemVoList().get(FIRST);
    if (itemVo.getMasterDataItem() != null) {
      itemVo.setMasterDataItem(this.masterDataConstructorService
          .constructItemDimensionFields(itemVo.getMasterDataItem(), productVo.getMasterDataProduct()));
    }
    ProductInfoResponse productInfoResponse =
      gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class);
    ItemInfoResponseV2 itemInfoResponse = gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class);
    productInfoResponse.setPreOrder(checkPreOrder(productItemsVo.getProductVo(), true));
    return new ProductAndItemInfoResponseV2(productInfoResponse, itemInfoResponse);
  }

  @Override
  public List<PickupPointDetailResponse> convertToPickupPointDetailResponseList(
      List<BusinessPartnerPickupPoint> businessPartnerPickupPoints, boolean isFbb) {
    List<PickupPointDetailResponse> pickupPointDetailResponseList = new ArrayList<>();
    if (isFbb) {
      businessPartnerPickupPoints.stream().filter(BusinessPartnerPickupPoint::isFbbActivated).forEach(
          businessPartnerPickupPoint -> pickupPointDetailResponseList.add(
              PickupPointDetailResponse.builder().pickupPointCode(businessPartnerPickupPoint.getCode())
                  .pickupPointName(businessPartnerPickupPoint.getName()).build()));
    } else {
      businessPartnerPickupPoints.forEach(businessPartnerPickupPoint -> pickupPointDetailResponseList.add(
          PickupPointDetailResponse.builder().pickupPointCode(businessPartnerPickupPoint.getCode())
              .pickupPointName(businessPartnerPickupPoint.getName()).build()));
    }
    return pickupPointDetailResponseList;
  }

  private ProductAndItemsDTO toProductItemsVo(ProductItemsVo productItemsVo) {
    ProductResponse productResponse = this.gdnMapperHelper.mapBean(productItemsVo.getProductVo(),
      ProductResponse.class);
    List<ItemResponse> itemResponses = new ArrayList<>();
    for (ItemVo itemVo : productItemsVo.getItemVoList()) {
      ItemResponse itemResponse = this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class);
      itemResponses.add(itemResponse);
    }
    return new ProductAndItemsDTO(productResponse, itemResponses);
  }

  private List<BasicProductAttributeDetailsDTO> getDescriptiveAttributeFromMasterDataProductAttributes(
      List<MasterDataProductAttribute> masterDataProductAttributes,
    List<ProductSpecialAttribute> productSpecialAttributesList) {
    List<BasicProductAttributeDetailsDTO> basicProductAttributeDetailsDTOS = new ArrayList<>();
    List<ProductAttributeDetail> productAttributeDetails =
        fetchDescriptiveAttributeFromMasterDataProduct(masterDataProductAttributes, false);
    setSpecialAttributesFromProduct(masterDataProductAttributes, productAttributeDetails,
      productSpecialAttributesList);
    for (ProductAttributeDetail productAttributeDetail : productAttributeDetails) {
      basicProductAttributeDetailsDTOS.add(
          new BasicProductAttributeDetailsDTO(productAttributeDetail.getAttributeName(),
              productAttributeDetail.getAttributeValue(), productAttributeDetail.getattributeValueEnglish(),
              productAttributeDetail.isMustShow()));
    }
    return basicProductAttributeDetailsDTOS;
  }

  private void setSpecialAttributesFromProduct(
    List<MasterDataProductAttribute> masterDataProductAttributes,
    List<ProductAttributeDetail> productAttributeDetails,
    List<ProductSpecialAttribute> productSpecialAttributesList) {
    if (setSpecialAttributesInPDPApi && Objects.nonNull(masterDataProductAttributes)) {
      Map<String, ProductAttributeDetail> productAttributeDetailMap = productAttributeDetails.stream().collect(
          Collectors.toMap(ProductAttributeDetail::getAttributeCode, Functions.identity(),
              (existing, replacement) -> existing));
      Map<String, ProductSpecialAttribute> productSpecialAttributeMap =
        productSpecialAttributesList.stream().collect(
          Collectors.toMap(ProductSpecialAttribute::getAttributeCode, Functions.identity()));

      Map<String, MasterDataAttribute> masterDataAttributeMap =
        masterDataProductAttributes.stream().filter(Objects::nonNull)
          .map(MasterDataProductAttribute::getMasterDataAttribute)
          .collect(Collectors.toMap(MasterDataAttribute::getAttributeCode, Function.identity()));

      // Remove any special attributes that already exist in the product attribute details.
      productSpecialAttributeMap.entrySet().removeIf(
        specialAttributeEntry -> productAttributeDetailMap.containsKey(
          specialAttributeEntry.getKey()));
      Set<MasterDataAttributeType> validAttributeTypes =
        Set.of(MasterDataAttributeType.PREDEFINED_ATTRIBUTE,
          MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
      for (Entry<String, ProductSpecialAttribute> productSpecialAttributeEntry : productSpecialAttributeMap.entrySet())
        setSpecialAttributesDetails(productAttributeDetails, productSpecialAttributeEntry, masterDataAttributeMap,
          validAttributeTypes);
    }
  }

  private static void setSpecialAttributesDetails(List<ProductAttributeDetail> productAttributeDetails,
    Entry<String, ProductSpecialAttribute> productSpecialAttributeEntry,
    Map<String, MasterDataAttribute> masterDataAttributeMap,
    Set<MasterDataAttributeType> validAttributeTypes) {
    String attributeCode = productSpecialAttributeEntry.getKey();
    ProductSpecialAttribute productSpecialAttribute = productSpecialAttributeEntry.getValue();
    MasterDataAttribute masterDataAttribute = masterDataAttributeMap.get(attributeCode);
    // set data only if Attribute is present in PCB and XProduct both
    if (Objects.nonNull(masterDataAttribute) && !masterDataAttribute.isMarkForDelete() && validAttributeTypes.contains(
      masterDataAttribute.getAttributeType())) {
      productAttributeDetails.add(
        new ProductAttributeDetail(attributeCode, productSpecialAttribute.getAttributeName(),
          productSpecialAttribute.getAttributeValue(), masterDataAttribute.isMustShow()));
    }
  }

  private List<ItemCatalogDTO> toItemCatalogDTOs (List<ItemCatalogVO> itemCatalogVOS) throws Exception {
    List<ItemCatalogDTO> itemCatalogDTOs = new ArrayList<>();
    for (ItemCatalogVO itemCatalogVO : itemCatalogVOS) {
      ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
      PropertyUtils.copyProperties(itemCatalogDTO, itemCatalogVO);
      itemCatalogDTOs.add(itemCatalogDTO);
    }
    return itemCatalogDTOs;
  }

  @Override
  public BasicProductAndItemResponse toBasicProductAndItemResponse(BasicProductAndItemDTO basicProductAndItemDTO, String fetchViewConfigByChannel) throws Exception {
    BasicProductAndItemResponse basicProductAndItemResponse = new BasicProductAndItemResponse();
    BasicProductDTO basicProductDTO = new BasicProductDTO();
    basicProductAndItemResponse.setItem(basicProductAndItemDTO.getItem());
    if (Optional.ofNullable(basicProductAndItemResponse.getItem()).isPresent()) {
      basicProductAndItemResponse.getItem().setCncActive(ResponseHelper.setCncActivatedForBackward(
          basicProductAndItemResponse.getItem().getItemViewConfigs(), cncForWarehouseFeatureSwitch,
          basicProductAndItemResponse.getItem().isCncActive(), ItemViewConfigDTO::getChannel,
          ItemViewConfigDTO::isBuyable));
      validateAndUpdateViewConfigs(basicProductAndItemResponse.getItem(), fetchViewConfigByChannel);
    }
    basicProductDTO.setMerchantCode(basicProductAndItemDTO.getMerchantCode());
    basicProductDTO.setProductSku(basicProductAndItemDTO.getProductSku());
    basicProductDTO.setProductCode(basicProductAndItemDTO.getProductCode());
    basicProductDTO.setProductType(basicProductAndItemDTO.getProductType());
    basicProductDTO.setMasterCatalog(basicProductAndItemDTO.getMasterCatalog());
    basicProductDTO.setMasterDataProduct(basicProductAndItemDTO.getMasterDataProduct());
    basicProductDTO.setItemCatalogs(toItemCatalogDTOs(basicProductAndItemDTO.getItemCatalogVOS()));
    basicProductDTO.setDescriptiveAttributes(getDescriptiveAttributeFromMasterDataProductAttributes(
        basicProductAndItemDTO.getMasterDataProductAttributes(), basicProductAndItemDTO.getProductSpecialAttributesList()));
    Product product = new Product();
    product.setPreOrder(basicProductAndItemDTO.getPreOrder());
    basicProductDTO.setPreOrder(checkPreOrder(product, true));
    basicProductAndItemResponse.setProduct(basicProductDTO);
    return basicProductAndItemResponse;
  }

  @Override
  public SizeChartResponse updateSizeChartResponse(SizeChartResponse sizeChartResponse) {
    if (CollectionUtils.isNotEmpty(sizeChartResponse.getSizeChartRows())) {
      for (SizeChartDataRow sizeChartDataRow : sizeChartResponse.getSizeChartRows()) {
        if (CollectionUtils.isNotEmpty(sizeChartDataRow.getColumns())) {
          for (SizeChartDataColumn sizeChartDataColumn : sizeChartDataRow.getColumns()) {
            setDefaultValueIfEmpty(sizeChartDataColumn);
          }
        }
      }
    }
    return sizeChartResponse;
  }

  private static void setDefaultValueIfEmpty(SizeChartDataColumn sizeChartDataColumn) {
    if (StringUtils.isBlank(sizeChartDataColumn.getValue())) {
      sizeChartDataColumn.setValue(Constants.ZERO);
    }
    if (StringUtils.isBlank(sizeChartDataColumn.getMax())) {
      sizeChartDataColumn.setMax(Constants.ZERO);
    }
    if (StringUtils.isBlank(sizeChartDataColumn.getMin())) {
      sizeChartDataColumn.setMin(Constants.ZERO);
    }
  }
}
