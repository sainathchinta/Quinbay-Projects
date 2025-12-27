package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.domain.event.model.BusinessHourVOEventModel;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.rest.web.model.response.CategoryDataResponse;
import com.gdn.x.product.rest.web.model.response.CategoryHierarchyDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.product.domain.event.model.OdooPickupPointModel;
import com.gdn.x.product.domain.event.model.ProductItem;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.DiscountPriceModel;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.PriceModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.domain.event.model.SalesCatalogModel;
import com.gdn.x.product.enums.AdjustmentTypeEnum;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.DayOfWeek;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessHour;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Geolocation;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAllowedAttributeValue;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.AttributeVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboRuleVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemV2;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataItemAttributeVO;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.model.vo.PristineItemVO;
import com.gdn.x.product.model.vo.PristineSimilarItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingVO;
import com.gdn.x.product.model.vo.SimpleAsyncMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleItem2VO;
import com.gdn.x.product.model.vo.SimpleItemPickupPointVO;
import com.gdn.x.product.model.vo.SimpleItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimplePristineDataItemVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleProductVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.NewlyAddedL5Response;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicMasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BundleItemResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataProductV2;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponseV2;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ObjectConverterUtil;
import com.gdn.x.product.service.util.PriceUtil;
import com.gdn.x.product.service.util.ProductUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;

@Service
public class ObjectConverterServiceImpl implements ObjectConverterService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ObjectConverterServiceImpl.class);
  private static final String PRODUCT_ITEM_DETAIL_RESPONSE_MUST_NOT_BE_NULL =
      "productItemDetailResponse must not be null";
  private static final String PRODUCT_MUST_NOT_BE_NULL = "product must not be null";
  private static final String ITEM_MUST_NOT_BE_NULL = "item must not be null";
  private static final String MAP_OF_ITEM_CATEGORY_VO_LIST_MUST_NOT_BE_NULL = "mapOfCategoryVoList must not be null";
  private static final String CATEGORY_LIST_MUST_NOT_BE_NULL = "categoryCodeList must not be null";
  private static final String PRODUCT_DETAIL_RESPONSE_MUST_NOT_BE_NULL = "productDetailResponse must not be null";
  private static final String CATALOG_CODE_MASTER = "10001";
  private static final String PRODUCT_CATEGORY_RESPONSES_MUST_NOT_BE_NULL = "productCategoryResponses must not be null";
  private static final String PRICE_MUST_NOT_BE_NULL = "price must not be null";
  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";
  private static final List<String> GUARANTEE_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("garansi", "guarantee")));
  private static final List<String> GUARANTEE_DURATION_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("lama garansi")));
  private static final char SPACE = ' ';
  private static final String MASTER_PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL = "masterProductAndItemsVo must not be null";
  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "itemCode must not be blank";
  private static final String WAREHOUSE = "WH";
  private static final String MARKETPLACE = "MKT";
  public static final double DEFAULT_DOUBLE_VALUE = 0.0;

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseClient;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ItemPriceService itemPriceService;

  @Autowired
  private GdnMapper gdnMapper;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  @Lazy
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${master.category.catalog.code}")
  private String masterCatalogCode;

  @Value("${override.latefulfillment.by.product.type}")
  private boolean overrideLateFulfillmentByProductType;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${imei.attribute.code}")
  private String imeiAttributeCode;

  @Value("#{'${imei.attribute.allowed.values}'.split(',')}")
  private List<String> imeiAllowedValues;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Override
  public ProductAttribute convertPristineAttributeToProductAttribute(PristineDataItem pristineDataItem) {
    return ObjectConverterUtil.convertPristineAttributeToProductAttribute(pristineDataItem);
  }

  @Override
  public List<ProductAndItemsVO> convertAndValidateMasterDataExists(List<Product> products, List<Item> items, Map<String, MasterDataProduct> mapOfMasterDataProducts,
      Map<String, MasterDataItem> mapOfMasterDataItems) {
    Map<String, Product> mapOfProducts = new HashMap<>();
    for (Product product : products) {
      MasterDataProduct masterDataProduct = mapOfMasterDataProducts.get(product.getProductCode());
      if (product.isSynchronized() && masterDataProduct == null) {
        ObjectConverterServiceImpl.LOGGER.warn("#productGotProductCodeNotFound productSku:{},productCode:{}", product.getProductSku(),
            product.getProductCode());
        continue;
      }
      if (masterDataProduct != null) {
        product.setMasterCatalog(masterDataProduct.getMasterCatalog());
        if (!product.isSynchronized() && product.getMasterDataProduct() != null) {
          product.getMasterDataProduct().setMasterCatalog(masterDataProduct.getMasterCatalog());
        } else {
          product.setMasterDataProduct(masterDataProduct);
        }
      }
      mapOfProducts.put(product.getProductSku(), product);
    }
    Map<String, ProductAndItemsVO> mapOfResponse = new HashMap<>();

    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        Product product = mapOfProducts.get(item.getProductSku());
        if (product == null) {
          continue;
        }
        ProductAndItemsVO response = mapOfResponse.get(item.getProductSku());
        if (response == null) {
          response = new ProductAndItemsVO();
          response.setProduct(product);
          mapOfResponse.put(item.getProductSku(), response);
        }
        MasterDataItem masterDataItem = mapOfMasterDataItems.get(item.getItemCode());
        if (!product.isSynchronized()) {
          if (item.getMasterDataItem() != null && masterDataItem != null) {
            item.getMasterDataItem().setDangerousLevel(masterDataItem.getDangerousLevel());
          }
        } else if (masterDataItem != null) {
          item.setMasterDataItem(masterDataItem);
        }
        response.getItems().add(item);
      }
    }
    return new ArrayList<>(mapOfResponse.values());
  }

  @Override
  public List<SimpleProductAndItemsVO> convertAndValidateSimpleMasterDataExists(List<Product> products, List<Item> items,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts,
      Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems) {
    Map<String, SimpleProductVO> mapOfProducts = new HashMap<>();
    for (Product product : products) {
      SimpleMasterDataProductVO masterDataProduct = mapOfMasterDataProducts.get(product.getProductCode());
      if (Objects.isNull(masterDataProduct) && product.isSynchronized()) {
        ObjectConverterServiceImpl.LOGGER.warn("Master product not found for productSku {}, productCode :{}", product.getProductSku(), product.getProductCode());
        continue;
      }
      if (Objects.nonNull(masterDataProduct) && CollectionUtils.isEmpty(masterDataProduct.getSalesCatalogs())) {
        masterDataProduct.setSalesCatalogs(product.getSalesCatalogs());
        masterDataProduct.setSalesCategorySequences(product.getSalesCategorySequences());
      }
      SimpleProductVO simpleProductVo = SimpleProductVO.toSimpleProductVo(masterDataProduct, product);
      simpleProductVo.setPreOrder(ObjectConverterUtil.checkPreOrder(product));
      mapOfProducts.put(product.getProductSku(), simpleProductVo);
    }
    return createSimpleProductAndItems(mapOfProducts, items, mapOfMasterDataItems);
  }

  @Override
  public List<SimpleProductAndItemsAndItemPickupPointV0> convertAndValidateSimpleMasterDataExistsV2(
      List<Product> products, List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts, Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems) {
    Map<String, SimpleProductVO> mapOfProducts = new HashMap<>();
    for (Product product : products) {
      SimpleMasterDataProductVO masterDataProduct = mapOfMasterDataProducts.get(product.getProductCode());
      if (Objects.isNull(masterDataProduct) && product.isSynchronized()) {
        ObjectConverterServiceImpl.LOGGER.warn("Master product not found for productSku {}, productCode :{}", product.getProductSku(), product.getProductCode());
        continue;
      }
      if (Objects.nonNull(masterDataProduct) && CollectionUtils.isEmpty(masterDataProduct.getSalesCatalogs())) {
        masterDataProduct.setSalesCatalogs(product.getAllSalesCatalogs());
        masterDataProduct.setSalesCategorySequences(product.getSalesCategorySequences());
      }
      SimpleProductVO simpleProductVo = SimpleProductVO.toSimpleProductVo(masterDataProduct, product);
      simpleProductVo.setPreOrder(ObjectConverterUtil.checkPreOrder(product));
      simpleProductVo.setHalalProduct(CurationStatus.APPROVED.equals(product.getCurationStatus()));
      simpleProductVo.setSizeChartCode(product.getSizeChartCode());
      if (Objects.nonNull(masterDataProduct)) {
        simpleProductVo.setCategoryCodeAndCategoryNameMap(masterDataProduct.getCategoryCodeAndCategoryNameMap());
      }
      mapOfProducts.put(product.getProductSku(), simpleProductVo);
    }
    return createSimpleProductAndItemsAndItemPickupPoint(mapOfProducts, items, itemPickupPoints, mapOfMasterDataItems);
  }

  private boolean checkPreOrderDateStatus(Date preOrderDate) {
    return ObjectConverterUtil.checkPreOrderDateStatus(preOrderDate);
  }

  private List<SimpleProductAndItemsVO> createSimpleProductAndItems(Map<String, SimpleProductVO> mapOfProducts, List<Item> items, Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems) {
    Map<String, SimpleProductAndItemsVO> mapOfResponse = new HashMap<>();
    if (CollectionUtils.isNotEmpty(items)) {
      items.forEach(item -> {
        SimpleProductVO simpleProductVo = mapOfProducts.get(item.getProductSku());
        if (Objects.nonNull(simpleProductVo)) {
          SimpleItemVO simpleItemVo = SimpleItemVO.toSimpleItemVo(item, mapOfMasterDataItems.get(item.getItemCode()));
          if (Objects.nonNull(simpleItemVo.getSimplePristineDataItem()) && StringUtils.isBlank(
              simpleItemVo.getSimplePristineDataItem().getPristineBrand())) {
            LOGGER.info("Fetching the pristine details from solr and PCB for item Sku : {}", item.getItemSku());
            ProductAndItemSolr productAndItemSolr = productAndItemSolrRepository.findOne(item.getItemSku(),
              item.getMerchantCode());
            simpleItemVo.getSimplePristineDataItem().setPristineBrand(productAndItemSolr.getBrand());
            simpleItemVo.getSimplePristineDataItem().setPristineProductName(productAndItemSolr.getItemName());
            if (StringUtils.isNotBlank(productAndItemSolr.getMasterCatalog())) {
              String categoryCode = productAndItemSolr.getMasterCatalog().split(Constants.DELIMETER)[1];
              CategoryNamesResponse categoryNamesResponse = productCategoryBaseClient.getCategoryNames(Arrays.asList(categoryCode));
              if (Objects.nonNull(categoryNamesResponse)) {
                simpleItemVo.getSimplePristineDataItem()
                    .setPristineCategory(categoryNamesResponse.getCategoryMap().get(categoryCode));
              }
            }
          }
          SimpleProductAndItemsVO response = mapOfResponse.get(simpleItemVo.getProductSku());
          if (Objects.isNull(response)) {
            response = new SimpleProductAndItemsVO();
            response.setSimpleProduct(simpleProductVo);
            mapOfResponse.put(item.getProductSku(), response);
          }
          response.getSimpleItems().add(simpleItemVo);
        }
      });
    }
    return new ArrayList<>(mapOfResponse.values());
  }

  private List<SimpleProductAndItemsAndItemPickupPointV0> createSimpleProductAndItemsAndItemPickupPoint(
      Map<String, SimpleProductVO> mapOfProducts, List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      Map<String, SimpleMasterDataItemVO> mapOfMasterDataItems) {
    Map<String, SimpleProductAndItemsAndItemPickupPointV0> mapOfResponse = new HashMap<>();
    if (CollectionUtils.isNotEmpty(items)) {
      items.forEach(item -> {
        SimpleProductVO simpleProductVo = mapOfProducts.get(item.getProductSku());
        if (Objects.nonNull(simpleProductVo)) {
          SimpleItem2VO simpleItemVo = toSimpleItem2Vo(item, mapOfMasterDataItems.get(item.getItemCode()));
          if (Objects.nonNull(simpleItemVo.getSimplePristineDataItem()) && StringUtils.isBlank(
              simpleItemVo.getSimplePristineDataItem().getPristineBrand())) {
            simpleItemVo.getSimplePristineDataItem().setPristineBrand(item.getBrand());
            simpleItemVo.getSimplePristineDataItem().setPristineProductName(item.getGeneratedItemName());
            if (MapUtils.isNotEmpty(simpleProductVo.getCategoryCodeAndCategoryNameMap())) {
              simpleItemVo.getSimplePristineDataItem()
                  .setPristineCategory(simpleProductVo.getCategoryCodeAndCategoryNameMap().get(item.getCategoryCode()));
            }
          }
          SimpleProductAndItemsAndItemPickupPointV0 response = mapOfResponse.get(simpleItemVo.getProductSku());
          if (Objects.isNull(response)) {
            response = new SimpleProductAndItemsAndItemPickupPointV0();
            response.setSimpleProduct(simpleProductVo);
            mapOfResponse.put(item.getProductSku(), response);
          }
          response.getSimpleItems().add(simpleItemVo);
        }
      });
    }
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      itemPickupPoints.forEach(itemPickupPoint -> {
        SimpleProductVO simpleProductVo = mapOfProducts.get(itemPickupPoint.getProductSku());
        if (Objects.nonNull(simpleProductVo)) {
          SimpleItemPickupPointVO simpleItemPickupPointVO = toSimpleItemPickupPointVO(itemPickupPoint, simpleProductVo);
          SimpleProductAndItemsAndItemPickupPointV0 response = mapOfResponse.get(simpleItemPickupPointVO.getProductSku());
          if (Objects.isNull(response)) {
            response = new SimpleProductAndItemsAndItemPickupPointV0();
            response.setSimpleProduct(simpleProductVo);
            mapOfResponse.put(simpleItemPickupPointVO.getProductSku(), response);
          }
          response.getItemPickupPoints().add(simpleItemPickupPointVO);
        }
      });
    }
    return new ArrayList<>(mapOfResponse.values());
  }

  private SimpleItem2VO toSimpleItem2Vo(Item item, SimpleMasterDataItemVO masterDataItemVO) {
    SimpleItem2VO simpleItemVO = new SimpleItem2VO();
    simpleItemVO.setItemSku(item.getItemSku());
    simpleItemVO.setItemCode(item.getItemCode());
    simpleItemVO.setSynchronized(item.isSynchronized());
    simpleItemVO.setArchived(item.isArchived());
    simpleItemVO.setCncActivated(item.isCncActivated());
    simpleItemVO.setProductSku(item.getProductSku());
    simpleItemVO.setOff2OnChannelActive(item.isOff2OnChannelActive());
    simpleItemVO.setActivePromoBundlings(item.getActivePromoBundlings());
    simpleItemVO.setCreatedDate(item.getCreatedDate());
    simpleItemVO.setSubscribable(item.isSubscribable());
    simpleItemVO.setLength(item.getLength());
    simpleItemVO.setWidth(item.getWidth());
    simpleItemVO.setHeight(item.getHeight());
    simpleItemVO.setWeight(item.getWeight());
    simpleItemVO.setShippingWeight(item.getShippingWeight());
    simpleItemVO.setPreferredSubscriptionType(item.getPreferredSubscriptionType());
    simpleItemVO.setMasterSku(item.getMasterSku());
    if (Objects.nonNull(item.getPristineDataItem())) {
      simpleItemVO.setSimplePristineDataItem(
          SimplePristineDataItemVO.toSimplePristineDataItemVO(item.getPristineDataItem()));
    }
    if (!item.isSynchronized() && Objects.isNull(item.getPristineDataItem())) {
      SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItemVO =
          SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item);
      if (Objects.nonNull(masterDataItemVO) && CollectionUtils.isNotEmpty(masterDataItemVO.getMasterDataItemImages())) {
        simpleAsyncMasterDataItemVO.setMasterDataItemImages(masterDataItemVO.getMasterDataItemImages());
      }
      simpleItemVO.setSimpleAsyncMasterDataItem(simpleAsyncMasterDataItemVO);
    }
    return simpleItemVO;
  }

  private SimpleItemPickupPointVO toSimpleItemPickupPointVO(ItemPickupPoint itemPickupPoint, SimpleProductVO simpleProductVO) {
    SimpleItemPickupPointVO simpleItemPickupPointVO = new SimpleItemPickupPointVO();
    simpleItemPickupPointVO.setUniqueId(itemPickupPoint.getOfflineItemId());
    simpleItemPickupPointVO.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    simpleItemPickupPointVO.setItemSku(itemPickupPoint.getItemSku());
    simpleItemPickupPointVO.setProductSku(itemPickupPoint.getProductSku());
    simpleItemPickupPointVO.setPromoBundling(itemPickupPoint.isPromoBundling());
    simpleItemPickupPointVO.setPrices(itemPickupPoint.getPrice());
    simpleItemPickupPointVO.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs());
    simpleItemPickupPointVO.setCncActive(itemPickupPoint.isCncActive());
    simpleItemPickupPointVO.setArchived(simpleProductVO.isArchived());
    simpleItemPickupPointVO.setSuspended(simpleProductVO.isSuspended());
    simpleItemPickupPointVO.setOff2OnActive(simpleProductVO.isOff2OnActive());
    simpleItemPickupPointVO.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    simpleItemPickupPointVO.setFbbActivated(itemPickupPoint.isFbbActivated());
    simpleItemPickupPointVO.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    simpleItemPickupPointVO.setB2bFields(itemPickupPoint.getB2bFields());
    return simpleItemPickupPointVO;
  }

  @Override
  public List<ProductAndItemsVO> convertAndValidateMasterDataExistsForPristine(List<Product> products, List<Item> items,
      Map<String, MasterDataProduct> mapOfMasterDataProducts, Map<String, MasterDataItem> mapOfMasterDataItems) {
    Map<String, Product> mapOfProducts = new HashMap<>();
    MasterDataProduct masterDataProduct = null;
    if (MapUtils.isNotEmpty(mapOfMasterDataProducts)) {
      masterDataProduct = mapOfMasterDataProducts.entrySet().iterator().next().getValue();
    }
    for (Product product : products) {
      if (masterDataProduct != null) {
        product.setMasterCatalog(masterDataProduct.getMasterCatalog());
        if (product.getMasterDataProduct() != null) {
          product.getMasterDataProduct().setMasterCatalog(masterDataProduct.getMasterCatalog());
        }
      }
      mapOfProducts.put(product.getProductSku(), product);
    }
    Map<String, ProductAndItemsVO> mapOfResponse = new HashMap<>();
    for (Item item : items) {
      Product product = mapOfProducts.get(item.getProductSku());
      if (product == null) {
        continue;
      }
      ProductAndItemsVO response = mapOfResponse.get(item.getProductSku());
      if (response == null) {
        response = new ProductAndItemsVO();
        response.setProduct(product);
        mapOfResponse.put(item.getProductSku(), response);
      }
      if (!product.isSynchronized()) {
        MasterDataItem masterDataItem = mapOfMasterDataItems.get(item.getItemCode());
        if (masterDataItem != null && item.getMasterDataItem() != null) {
          item.getMasterDataItem().setDangerousLevel(masterDataItem.getDangerousLevel());
        }
      }
      response.getItems().add(item);
    }
    return new ArrayList<>(mapOfResponse.values());
  }

  @Override
  public PristineItemVO convertToPristineItem(Item item) {
    return gdnMapper.deepCopy(item, PristineItemVO.class);
  }

  @Override
  public String convertBytesToString(byte[] bytes) {
    return ObjectConverterUtil.convertBytesToString(bytes);
  }

  @Override
  public AddProductAndItemsResponseVo convertToAddProductAndItemsResponseVo(Product product, List<Item> items) {
    AddProductAndItemsResponseVo responseVo = new AddProductAndItemsResponseVo();
    responseVo.setMerchantCode(product.getMerchantCode());
    responseVo.setProductCode(product.getProductCode());
    responseVo.setProductSku(product.getProductSku());
    Map<String, String> mapOfItemSkuByItemCode = new HashMap<>();
    for (Item item : items) {
      mapOfItemSkuByItemCode.put(item.getItemCode(), item.getItemSku());
    }
    responseVo.setMapOfItemSkuByItemCode(mapOfItemSkuByItemCode);
    return responseVo;
  }

  @Override
  public AddProductAndItemsResponseVo convertToAddProductItemsResponseVo(Product product, List<ItemVo> items) {
    AddProductAndItemsResponseVo responseVo = new AddProductAndItemsResponseVo();
    responseVo.setMerchantCode(product.getMerchantCode());
    responseVo.setProductCode(product.getProductCode());
    responseVo.setProductSku(product.getProductSku());
    Map<String, String> mapOfItemSkuByItemCode = new HashMap<>();
    for (Item item : items) {
      mapOfItemSkuByItemCode.put(item.getItemCode(), item.getItemSku());
    }
    responseVo.setMapOfItemSkuByItemCode(mapOfItemSkuByItemCode);
    return responseVo;
  }

  private ProductItemDetailVO convertToItemDetail(Product product, Item item, List<ItemCatalogVO> itemCatalogs) {
    ProductItemDetailVO itemDetail = new ProductItemDetailVO();
    setProductItemDetailVO(itemDetail, product, item, itemCatalogs);
    if (Objects.isNull(product.getMasterDataProduct()) || Objects.isNull(item.getMasterDataItem())) {
      itemDetail.setBuyable(false);
      itemDetail.setDiscoverable(false);
      return itemDetail;
    }
    itemDetail.setBrandName(product.getMasterDataProduct().getBrand());
    itemDetail.setItemName(item.getMasterDataItem().getGeneratedItemName());
    item.setMasterDataItem(this.masterDataConstructorService.constructItemDimensionFields(item.getMasterDataItem(),
        product.getMasterDataProduct()));
    itemDetail.setItemLength(item.getMasterDataItem().getItemLength());
    itemDetail.setItemWidth(item.getMasterDataItem().getItemWidth());
    itemDetail.setItemHeight(item.getMasterDataItem().getItemHeight());
    itemDetail.setShippingWeight(item.getMasterDataItem().getItemDeliveryWeight());
    itemDetail.setItemWeight(item.getMasterDataItem().getItemWeight());
    itemDetail.setDangerousLevel(item.getMasterDataItem().getDangerousLevel());
    itemDetail.setOff2OnChannelActive(item.isOff2OnChannelActive());
    itemDetail.setProductName(product.getMasterDataProduct().getProductName());
    if (Objects.nonNull(item.getPristineDataItem())) {
      itemDetail.setPristineId(item.getPristineDataItem().getPristineId());
      if (StringUtils.isEmpty(item.getPristineDataItem().getPristineModel()) && StringUtils.isEmpty(
          item.getPristineDataItem().getPristineBrand())) {
        itemDetail.setProductName(item.getMasterDataItem().getGeneratedItemName());
      } else {
        itemDetail.setProductName(item.getPristineDataItem().getPristineProductName());
      }
    }
    itemDetail.setImageUrl(item.getMasterDataItem().getMasterDataItemImages().stream().filter(masterDataItemImage -> masterDataItemImage.isMainImage()).findFirst().orElse(new MasterDataItemImage())
        .getLocationPath());
    itemDetail.setMasterDataItemAttributes(convertMasterDataItemAttributeToMasterDataItemAttributeVO(
        item.getMasterDataItem().getMasterDataItemAttributeValues()));
    itemDetail.setLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
            item.isLateFulfillment()));
    return itemDetail;
  }

  private ProductItemDetailVO convertToItemDetailV2(Product product, Item item, List<ItemCatalogVO> itemCatalogs) {
    ProductItemDetailVO itemDetail = new ProductItemDetailVO();
    if (item.isSynchronized()) {
      setProductItemDetailVO(itemDetail, product, item, itemCatalogs);
      itemDetail.setBrandName(item.getBrand());
      itemDetail.setItemName(item.getGeneratedItemName());
      itemDetail.setItemLength(item.getLength());
      itemDetail.setItemWidth(item.getWidth());
      itemDetail.setItemHeight(item.getHeight());
      itemDetail.setShippingWeight(item.getShippingWeight());
      itemDetail.setItemWeight(item.getWeight());
      itemDetail.setDangerousLevel(
        Objects.nonNull(item.getDangerousLevel()) ? item.getDangerousLevel() : 0);
      itemDetail.setOff2OnChannelActive(item.isOff2OnChannelActive());
      itemDetail.setProductName(product.getProductName());
      if (Objects.nonNull(item.getPristineDataItem())) {
        itemDetail.setPristineId(item.getPristineDataItem().getPristineId());
        if (StringUtils.isEmpty(item.getPristineDataItem().getPristineModel()) && StringUtils.isEmpty(
            item.getPristineDataItem().getPristineBrand())) {
          itemDetail.setProductName(item.getGeneratedItemName());
        } else {
          itemDetail.setProductName(item.getPristineDataItem().getPristineProductName());
        }
      }
      itemDetail.setImageUrl(item.getMainImageUrl());
      itemDetail.setMasterDataItemAttributes(
          convertProductAttributeDetailToMasterDataItemAttributeVO(item.getDefiningAttributes()));
    } else {
      itemDetail = convertToItemDetail(product, item, itemCatalogs);
    }
    return itemDetail;
  }

  @Override
  public ItemSummaryPageResponseVo convertToItemSummary(ProductAndItemsVO productAndItems) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    Product product = productAndItems.getProduct();
    for (Item item : productAndItems.getItems()) {
      ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
      if (!item.isSynchronized()) {
        itemSummaryResponse.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
        itemSummaryResponse.setMasterDataItemImages(item.getMasterDataItem().getMasterDataItemImages());
      }
      itemSummaryResponse.setItemCode(item.getItemCode());
      itemSummaryResponse.setItemSku(item.getItemSku());
      itemSummaryResponse.setItemViewConfigs(item.getItemViewConfigs());
      itemSummaryResponse.setIsLateFulfillment(item.isLateFulfillment());
      itemSummaryResponse.setMasterCatalog(product.getMasterCatalog());
      itemSummaryResponse.setMerchantCode(product.getMerchantCode());
      itemSummaryResponse.setMerchantSku(item.getMerchantSku());
      itemSummaryResponse.setPickupPointCode(item.getPickupPointCode());
      itemSummaryResponse.setTicketTemplateCode(item.getTicketTemplateCode());
      itemSummaryResponse.setPrice(item.getPrice());
      itemSummaryResponse.setProductSku(item.getProductSku());
      itemSummaryResponse.setProductType(product.getProductType());
      itemSummaryResponse.setSalesCatalogs(product.getSalesCatalogs());
      itemSummaryResponse.setOff2OnChannelActive(item.isOff2OnChannelActive());
      itemSummaryResponse.setProductCode(product.getProductCode());
      itemSummaryResponse.setBrand(product.getMasterDataProduct().getBrand());
      itemSummaryResponse.setArchived(item.isArchived());
      itemSummaryResponse.setMarkForDelete(item.isMarkForDelete());
      itemSummaryResponses.add(itemSummaryResponse);
    }
    return new ItemSummaryPageResponseVo(itemSummaryResponses, productAndItems.getItems().size(),
        productAndItems.getItems().size());
  }

  @Override
  public List<ItemCatalogVO> convertToListOfItemCatalog(List<List<CategoryResponse>> listOfCategoriesList) {
    return ObjectConverterUtil.convertToListOfItemCatalog(listOfCategoriesList);
  }

  @Override
  public List<ItemCatalogVOV2> convertToListOfItemCatalogV2(List<List<CategoryResponse>> listOfCategoriesList) {
    return ObjectConverterUtil.convertToListOfItemCatalogV2(listOfCategoriesList);
  }

  @Override
  public MasterCatalog convertToMasterCatalog(List<ProductCategoryResponse> productCategoryResponses) {
    checkArgument(productCategoryResponses != null,
        ObjectConverterServiceImpl.PRODUCT_CATEGORY_RESPONSES_MUST_NOT_BE_NULL);

    List<CategoryResponse> categories = new ArrayList<>();
    for (ProductCategoryResponse productCategoryResponse : productCategoryResponses) {
      if (ObjectConverterServiceImpl.CATALOG_CODE_MASTER.equals(
          productCategoryResponse.getCategory().getCatalog().getCatalogCode())) {
        categories.add(productCategoryResponse.getCategory());
      }
    }

    String rootCategoryCode = "";
    for (CategoryResponse categoryResponse : categories) {
      if (categoryResponse.getParentCategoryId() == null) {
        rootCategoryCode = categoryResponse.getId();
      }
    }

    String lowestCategoryCode = rootCategoryCode;
    for (int index = 0; index < categories.size(); index++) {
      CategoryResponse currentCategory = categories.get(index);
      if (lowestCategoryCode.equals(currentCategory.getParentCategoryId())) {
        lowestCategoryCode = currentCategory.getId();
        index = 0;
      }
    }

    Category category = new Category();
    category.setCategoryCode(lowestCategoryCode);
    category.setCatgroupId(lowestCategoryCode);

    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(ObjectConverterServiceImpl.CATALOG_CODE_MASTER);
    masterCatalog.setCategory(category);

    return masterCatalog;
  }

  @Override
  public MasterCatalog convertToMasterCatalogFromDirectParentCategory(CategoryDetailResponse categoryDetailResponse) {
    checkArgument(categoryDetailResponse != null,
        ObjectConverterServiceImpl.PRODUCT_CATEGORY_RESPONSES_MUST_NOT_BE_NULL);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(categoryDetailResponse.getCatalog().getCatalogCode());
    Category category = new Category();
    category.setCategoryCode(categoryDetailResponse.getCategoryCode());
    category.setCatgroupId(categoryDetailResponse.getCategoryCode());
    masterCatalog.setCategory(category);
    return masterCatalog;
  }

  @Override
  public MasterDataAttribute convertToMasterDataAttribute(AttributeResponse attributeResponse) {
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(attributeResponse.getAttributeCode());
    masterDataAttribute.setAttributeName(attributeResponse.getName());
    masterDataAttribute.setAttributeType(
        MasterDataAttributeType.valueOf(String.valueOf(attributeResponse.getAttributeType())));
    masterDataAttribute.setDescription(ObjectConverterUtil.convertBytesToString(attributeResponse.getDescription()));
    masterDataAttribute.setSearchable(attributeResponse.isSearchAble());
    masterDataAttribute.setSkuValue(attributeResponse.isSkuValue());
    masterDataAttribute.setStoreId(attributeResponse.getStoreId());
    masterDataAttribute.setVariantCreation(attributeResponse.isVariantCreation());
    masterDataAttribute.setBasicView(attributeResponse.isBasicView());
    masterDataAttribute.setMandatory(attributeResponse.isMandatory());
    masterDataAttribute.setExtractedValue(attributeResponse.isExtractedValue());
    masterDataAttribute.setSizeAttribute(attributeResponse.isSizeAttribute());
    masterDataAttribute.setMustShow(attributeResponse.isMustShowOnCustomerSide());
    masterDataAttribute.setId(attributeResponse.getId());
    masterDataAttribute.setHideOnCustomerSide(attributeResponse.isHideForCustomer());
    masterDataAttribute.setDsExtraction(attributeResponse.isDsExtraction());
    masterDataAttribute.setHideFromSeller(attributeResponse.isHideForSeller());
    return masterDataAttribute;
  }

  @Override
  public MasterDataItem convertToMasterDataItem(ProductItemDetailResponse productItemDetailResponse) {
    checkArgument(productItemDetailResponse != null,
        ObjectConverterServiceImpl.PRODUCT_ITEM_DETAIL_RESPONSE_MUST_NOT_BE_NULL);
    MasterDataItem masterDataItem = this.convertToMasterDataItem((ProductItemResponse) productItemDetailResponse);
    masterDataItem.setProductCode(productItemDetailResponse.getProductResponse().getProductCode());
    return masterDataItem;
  }

  public MasterDataItem convertToMasterDataItem(ProductItemResponse productItemResponse) {
    checkArgument(productItemResponse != null,
        ObjectConverterServiceImpl.PRODUCT_ITEM_DETAIL_RESPONSE_MUST_NOT_BE_NULL);

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setActivated(productItemResponse.isActivated());
    masterDataItem.setGeneratedItemName(productItemResponse.getGeneratedItemName());
    masterDataItem.setHash(ObjectConverterUtil.convertBytesToString(productItemResponse.getHash()));
    masterDataItem.setItemDeliveryWeight(productItemResponse.getItemDeliveryWeight());
    masterDataItem.setItemHeight(productItemResponse.getItemHeight());
    masterDataItem.setItemLength(productItemResponse.getItemLength());
    masterDataItem.setItemWeight(productItemResponse.getItemWeight());
    masterDataItem.setItemWidth(productItemResponse.getItemWidth());
    masterDataItem.setSkuCode(productItemResponse.getSkuCode());
    masterDataItem.setUpcCode(productItemResponse.getUpcCode());
    masterDataItem.setViewable(productItemResponse.isViewable());
    if (productItemResponse.getDangerousGoodsLevel() != null) {
      masterDataItem.setDangerousLevel(productItemResponse.getDangerousGoodsLevel());
    }
    List<MasterDataItemImage> itemImages = new ArrayList<>();

    for (Image image : productItemResponse.getImages()) {
      MasterDataItemImage itemImage = new MasterDataItemImage();
      itemImage.setLocationPath(image.getLocationPath());
      itemImage.setMainImage(image.isMainImages());
      itemImage.setSequence(image.getSequence());
      itemImages.add(itemImage);
    }

    masterDataItem.setMasterDataItemImages(itemImages);
    if (productItemResponse.getProductItemAttributeValueResponses() != null) {
      List<MasterDataItemAttributeValue> itemAttributeValues = new ArrayList<>();
      for (ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemResponse.getProductItemAttributeValueResponses()) {
        MasterDataItemAttributeValue masterDataItemAttributeValue = new MasterDataItemAttributeValue();
        MasterDataAttribute masterDataAttribute = this.convertToMasterDataAttribute(productItemAttributeValueResponse.getAttributeResponse());
        masterDataItemAttributeValue.setMasterDataAttribute(masterDataAttribute);
        masterDataItemAttributeValue.setAttributeValue(productItemAttributeValueResponse.getValue());
        itemAttributeValues.add(masterDataItemAttributeValue);
      }
      masterDataItem.setMasterDataItemAttributeValues(itemAttributeValues);
    }
    return masterDataItem;
  }

  @Override
  public Map<String, MasterDataItem> convertToMasterDataItems(Set<ProductItemResponse> productDetailResponses, String productCode) {
    Map<String, MasterDataItem> mapOfMasterDataItem = new HashMap<>();
    for (ProductItemResponse response : productDetailResponses) {
      MasterDataItem masterDataItem = this.convertToMasterDataItem(response);
      masterDataItem.setProductCode(productCode);
      mapOfMasterDataItem.put(masterDataItem.getSkuCode(), masterDataItem);
    }
    return mapOfMasterDataItem;
  }

  @Override
  public MasterDataProduct convertToMasterDataProduct(ProductDetailResponse productDetailResponse) {
    checkArgument(productDetailResponse != null, ObjectConverterServiceImpl.PRODUCT_DETAIL_RESPONSE_MUST_NOT_BE_NULL);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(productDetailResponse.getName());
    masterDataProduct.setBrand(productDetailResponse.getBrand());
    masterDataProduct.setShippingWeight(Objects.nonNull(productDetailResponse.getShippingWeight()) ?
      productDetailResponse.getShippingWeight() : 0.0);
    masterDataProduct.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    masterDataProduct.setDescription(ObjectConverterUtil.convertBytesToString(productDetailResponse.getDescription()));
    masterDataProduct.setLongDescription(ObjectConverterUtil.convertBytesToString(productDetailResponse.getLongDescription()));
    masterDataProduct.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    masterDataProduct.setActivated(productDetailResponse.isActivated());
    masterDataProduct.setViewable(productDetailResponse.isViewable());
    masterDataProduct.setProductStory(productDetailResponse.getProductStory());
    masterDataProduct.setUom(productDetailResponse.getUom());
    masterDataProduct.setUrl(productDetailResponse.getUrl());
    if (ranchIntegrationEnabled && Objects.nonNull(productDetailResponse.getDistributionInfoResponse())) {
      try {
        masterDataProduct.setDistributionInfo(
            objectMapper.writeValueAsString(productDetailResponse.getDistributionInfoResponse()));
      } catch (JsonProcessingException e) {
        LOGGER.error("Error when converting distribution info for productCode : {} ",
            productDetailResponse.getProductCode());
      }
    }

    Optional.ofNullable(productDetailResponse.getLength()).ifPresent(masterDataProduct::setLength);
    Optional.ofNullable(productDetailResponse.getWidth()).ifPresent(masterDataProduct::setWidth);
    Optional.ofNullable(productDetailResponse.getHeight()).ifPresent(masterDataProduct::setHeight);
    Optional.ofNullable(productDetailResponse.getWeight()).ifPresent(masterDataProduct::setWeight);
    masterDataProduct.setReviewPending(productDetailResponse.isReviewPending());

    MasterCatalog masterCatalog = new MasterCatalog();
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductCategoryResponses())) {
      CategoryResponse category = productDetailResponse.getProductCategoryResponses().get(0).getCategory();
      masterCatalog.setCatalogCode(category.getCatalog().getCatalogCode());
      masterCatalog.setCategory(new Category(category.getCategoryCode(), category.getCategoryCode()));
    }
    masterDataProduct.setMasterCatalog(masterCatalog);

    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();

      masterDataProductAttribute.setMasterDataAttribute(
          this.convertToMasterDataAttribute(productAttributeResponse.getAttribute()));
      masterDataProductAttribute.setOwnedByProductItem(productAttributeResponse.isOwnByProductItem());
      masterDataProductAttribute.setSequence(productAttributeResponse.getSequence());
      masterDataProductAttribute.setMarkForDelete(productAttributeResponse.isMarkForDelete());

      List<MasterDataProductAttributeValue> masterDataProductAttributeValues = new ArrayList<>();
      for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()) {
        MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
        AllowedAttributeValueResponse allowedAttributeValue = productAttributeValueResponse.getAllowedAttributeValue();
        if (allowedAttributeValue != null) {
          String allowedAttributeCode = allowedAttributeValue.getAllowedAttributeCode();
          masterDataProductAttributeValue.setAllowedAttributeValueCode(allowedAttributeCode);
          MasterDataAllowedAttributeValue masterDataAllowedAttributeValue = new MasterDataAllowedAttributeValue();
          masterDataAllowedAttributeValue.setAllowedAttributeValueCode(allowedAttributeCode);
          masterDataAllowedAttributeValue.setValue(allowedAttributeValue.getValue());
          masterDataAllowedAttributeValue.setSequence(allowedAttributeValue.getSequence());
          masterDataAllowedAttributeValue.setValueType(allowedAttributeValue.getValueType());
          masterDataProductAttributeValue.setAllowedAttributeValue(masterDataAllowedAttributeValue);
        }
        if (productAttributeValueResponse.getPredefinedAllowedAttributeValue() != null) {
          masterDataProductAttributeValue.setPredefinedAllowedAttributeValueCode(
              productAttributeValueResponse.getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
          PredefinedAllowedAttributeValue predefined = new PredefinedAllowedAttributeValue();
          predefined.setPredefinedAllowedAttributeCode(
              productAttributeValueResponse.getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
          predefined.setSequence(productAttributeValueResponse.getPredefinedAllowedAttributeValue().getSequence());
          predefined.setValue(productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue());
          predefined.setValueEn(productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValueEn());
          masterDataProductAttributeValue.setPredefinedAllowedAttributeValue(predefined);
        }
        masterDataProductAttributeValue.setDescriptiveAttributeValue(
            productAttributeValueResponse.getDescriptiveAttributeValue());
        masterDataProductAttributeValue.setMarkForDelete(productAttributeValueResponse.isMarkForDelete());
        if (productAttributeValueResponse.getDescriptiveAttributeValueType() != null) {
          masterDataProductAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.valueOf(
              productAttributeValueResponse.getDescriptiveAttributeValueType().name()));
        }
        masterDataProductAttributeValues.add(masterDataProductAttributeValue);
      }
      masterDataProductAttribute.setMasterDataProductAttributeValues(masterDataProductAttributeValues);
      masterDataProductAttributes.add(masterDataProductAttribute);
    }
    masterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
    setSizeAttributeCode(masterDataProductAttributes, masterDataProduct);

    List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
    for (Image image : productDetailResponse.getImages()) {
      MasterDataProductImage masterDataProductImage = new MasterDataProductImage();
      masterDataProductImage.setLocationPath(image.getLocationPath());
      masterDataProductImage.setMainImage(image.isMainImages());
      masterDataProductImage.setSequence(image.getSequence());
      masterDataProductImage.setCommonImage(image.isCommonImage());
      masterDataProductImages.add(masterDataProductImage);
    }
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProduct.setAiGeneratedFieldsResponse(ObjectConverterUtil.getAiGeneratedFields(productDetailResponse));
    return masterDataProduct;
  }

  private void setSizeAttributeCode(List<MasterDataProductAttribute> masterDataProductAttributes,
      MasterDataProduct masterDataProduct) {
    if (valueTypeAdditionForDefiningAttributes) {
      Optional<MasterDataAttribute> sizeAttribute = masterDataProductAttributes.stream().map(MasterDataProductAttribute::getMasterDataAttribute).filter(Objects::nonNull)
          .filter(MasterDataAttribute::isSizeAttribute).findFirst();
      sizeAttribute.ifPresent(masterDataAttribute -> masterDataProduct.setSizeAttributeCode(
          masterDataAttribute.getAttributeCode()));
    }
  }

  @Override
  public PriceHistory convertToPriceHistory(Price price, String itemSku) {
    return PriceUtil.convertToPriceHistory(price, itemSku);
  }

  @Override
  public ProductForTransactionVO convertToProductForTransaction(Product product, Item item,
      List<ItemCatalogVO> itemCatalogs) {
    checkArgument(product != null, ObjectConverterServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(item != null, ObjectConverterServiceImpl.ITEM_MUST_NOT_BE_NULL);
    checkArgument(itemCatalogs != null, ObjectConverterServiceImpl.MAP_OF_ITEM_CATEGORY_VO_LIST_MUST_NOT_BE_NULL);
    ProductItemDetailVO itemDetail = this.convertToItemDetail(product, item, itemCatalogs);
    ProductForTransactionVO productVO = new ProductForTransactionVO();
    productVO.setItemDetail(itemDetail);
    productVO.setItemSku(item.getItemSku());
    productVO.setItemCode(item.getItemCode());
    productVO.setCncActivated(item.isCncActivated());
    return productVO;
  }

  @Override
  public ProductForTransactionVO convertToProductForTransactionV2(Product product, Item item,
      List<ItemCatalogVO> itemCatalogs) {
    checkArgument(product != null, ObjectConverterServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(item != null, ObjectConverterServiceImpl.ITEM_MUST_NOT_BE_NULL);
    checkArgument(itemCatalogs != null, ObjectConverterServiceImpl.MAP_OF_ITEM_CATEGORY_VO_LIST_MUST_NOT_BE_NULL);
    ProductItemDetailVO itemDetail = this.convertToItemDetailV2(product, item, itemCatalogs);
    ProductForTransactionVO productVO = new ProductForTransactionVO();
    productVO.setItemDetail(itemDetail);
    productVO.setItemSku(item.getItemSku());
    productVO.setItemCode(item.getItemCode());
    productVO.setCncActivated(item.isCncActivated());
    return productVO;
  }

  @Override
  public List<ProductForTransactionVO> generateProductForTransactionVO(Set<String> productSkuWithoutMasterDataDetails,
      List<ProductAndItemsVO> productAndItemsVOList,
      Map<String, ProductMasterDataResponse> productMasterDataResponseMap) {
    List<ProductForTransactionVO> productForTransactionVOList = new ArrayList<>();
    for (ProductAndItemsVO productAndItemsVO : productAndItemsVOList) {
      if (productSkuWithoutMasterDataDetails.contains(productAndItemsVO.getProduct().getProductSku())) {
        productForTransactionVOList.add(convertToProductForTransactionNewMasterData(productAndItemsVO.getProduct(),
            productAndItemsVO.getItems().get(0), productAndItemsVO.getProduct().getItemCatalogs(),
            productMasterDataResponseMap.get(productAndItemsVO.getItems().get(0).getItemCode())));
      } else {
        productForTransactionVOList.add(
            convertToProductForTransactionV2(productAndItemsVO.getProduct(), productAndItemsVO.getItems().get(0),
                productAndItemsVO.getProduct().getItemCatalogs()));
      }
    }
    return productForTransactionVOList;
  }

  @Override
  public ProductForTransactionVO convertToProductForTransactionNewMasterData(Product product, Item item, List<ItemCatalogVO> itemCatalogVOS, ProductMasterDataResponse productMasterDataResponse) {
    checkArgument(Objects.nonNull(product), ObjectConverterServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(item), ObjectConverterServiceImpl.ITEM_MUST_NOT_BE_NULL);
    ProductItemDetailVO itemDetail;
    if (Objects.nonNull(productMasterDataResponse)) {
      itemDetail = this.convertToItemDetail(product, item, itemCatalogVOS, productMasterDataResponse);
    } else {
      itemDetail = this.convertToItemDetail(product, item, itemCatalogVOS);
    }
    ProductForTransactionVO productVO = new ProductForTransactionVO();
    productVO.setItemDetail(itemDetail);
    productVO.setItemSku(item.getItemSku());
    productVO.setItemCode(item.getItemCode());
    productVO.setCncActivated(item.isCncActivated());
    return productVO;
  }

  private ProductItemDetailVO convertToItemDetail(Product product, Item item, List<ItemCatalogVO> itemCatalogVOS,
      ProductMasterDataResponse productMasterDataResponse) {
    ProductItemDetailVO productItemDetailVO = new ProductItemDetailVO();
    //Set Common L3 L4 Detail
    setProductItemDetailVO(productItemDetailVO, product, item, itemCatalogVOS);
    //Set Master data detail
    ProductItemResponse productItemResponse = new ArrayList<>(productMasterDataResponse.getProductItemResponses()).get(0);
    if (product.isSynchronized()) {
      productItemDetailVO.setBrandName(productMasterDataResponse.getBrand());
      productItemDetailVO.setItemName(productItemResponse.getGeneratedItemName());
      productItemDetailVO.setProductName(productMasterDataResponse.getName());
      //Set item dimension if non-null, else product dimensions
      productItemDetailVO.setItemLength(Objects.nonNull(productItemResponse.getItemLength()) ?
          productItemResponse.getItemLength() :
          productMasterDataResponse.getLength());
      productItemDetailVO.setItemWidth(Objects.nonNull(productItemResponse.getItemWidth()) ?
          productItemResponse.getItemWidth() :
          productMasterDataResponse.getWidth());
      productItemDetailVO.setItemHeight(Objects.nonNull(productItemResponse.getItemHeight()) ?
          productItemResponse.getItemHeight() :
          productMasterDataResponse.getHeight());
      productItemDetailVO.setShippingWeight(Objects.nonNull(productItemResponse.getItemDeliveryWeight()) ?
          productItemResponse.getItemDeliveryWeight() :
          productMasterDataResponse.getShippingWeight());
      productItemDetailVO.setItemWeight(Objects.nonNull(productItemResponse.getItemWeight()) ?
          productItemResponse.getItemWeight() :
          productMasterDataResponse.getWeight());
    } else {
      productItemDetailVO.setBrandName(product.getMasterDataProduct().getBrand());
      productItemDetailVO.setItemName(item.getMasterDataItem().getGeneratedItemName());
      productItemDetailVO.setProductName(product.getMasterDataProduct().getProductName());
      //Set item dimension if non-null, else product dimensions
      productItemDetailVO.setItemLength(Objects.nonNull(item.getMasterDataItem().getItemLength()) ?
          item.getMasterDataItem().getItemLength() :
          product.getMasterDataProduct().getLength());
      productItemDetailVO.setItemWidth(Objects.nonNull(item.getMasterDataItem().getItemWidth()) ?
          item.getMasterDataItem().getItemWidth() :
          product.getMasterDataProduct().getWidth());
      productItemDetailVO.setItemHeight(Objects.nonNull(item.getMasterDataItem().getItemHeight()) ?
          item.getMasterDataItem().getItemHeight() :
          product.getMasterDataProduct().getHeight());
      productItemDetailVO.setShippingWeight(Objects.nonNull(item.getMasterDataItem().getItemDeliveryWeight()) ?
          item.getMasterDataItem().getItemDeliveryWeight() :
          product.getMasterDataProduct().getShippingWeight());
      productItemDetailVO.setItemWeight(Objects.nonNull(item.getMasterDataItem().getItemWeight()) ?
          item.getMasterDataItem().getItemWeight() :
          product.getMasterDataProduct().getWeight());
    }
    if (Objects.nonNull(item.getPristineDataItem())) {
      productItemDetailVO.setPristineId(item.getPristineDataItem().getPristineId());
      if (StringUtils.isBlank(item.getPristineDataItem().getPristineModel()) && StringUtils.isBlank(
          item.getPristineDataItem().getPristineBrand())) {
        if (item.isSynchronized()) {
          productItemDetailVO.setProductName(productItemResponse.getGeneratedItemName());
        } else {
          if (Objects.nonNull(item.getMasterDataItem())) {
            productItemDetailVO.setProductName(item.getMasterDataItem().getGeneratedItemName());
          }
        }
      } else {
        productItemDetailVO.setProductName(item.getPristineDataItem().getPristineProductName());
      }
    }
    productItemDetailVO.setImageUrl(
        productItemResponse.getImages().stream().filter(image -> image.isMainImages()).findFirst().orElse(new Image()).getLocationPath());
    productItemDetailVO.setDangerousLevel(Objects.nonNull(productItemResponse.getDangerousGoodsLevel()) ?
        productItemResponse.getDangerousGoodsLevel() :
        0);
    //Set Master data item attributes
    productItemDetailVO.setMasterDataItemAttributes(convertToMasterDataItemAttributeVO(productItemResponse.getProductItemAttributeValueResponses()));
    productItemDetailVO.setLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
            item.isLateFulfillment()));
    return productItemDetailVO;
  }

  private List<MasterDataItemAttributeVO> convertToMasterDataItemAttributeVO(List<ProductItemAttributeValueResponse> productItemAttributeValueResponses) {
    List<MasterDataItemAttributeVO> masterDataItemAttributes = new ArrayList<>();
    productItemAttributeValueResponses.stream().filter(productItemAttributeValueResponse -> StringUtils.equals(productItemAttributeValueResponse.getAttributeResponse().getAttributeType(),
            MasterDataAttributeType.DEFINING_ATTRIBUTE.name()) || productItemAttributeValueResponse.getAttributeResponse().isVariantCreation())
        .forEach(productItemAttributeValueResponse -> {
          MasterDataItemAttributeVO masterDataItemAttributeVO = new MasterDataItemAttributeVO();
          masterDataItemAttributeVO.setAttributeValue(productItemAttributeValueResponse.getValue());
          masterDataItemAttributeVO.setAttributeCode(
              productItemAttributeValueResponse.getAttributeResponse().getAttributeCode());
          masterDataItemAttributeVO.setAttributeName(productItemAttributeValueResponse.getAttributeResponse().getName());
          masterDataItemAttributes.add(masterDataItemAttributeVO);
        });
    return masterDataItemAttributes;
  }

  private void setProductItemDetailVO(ProductItemDetailVO itemDetail, Product product, Item item, List<ItemCatalogVO> itemCatalogs) {
    if (CollectionUtils.isNotEmpty(product.getProductSpecialAttributes())) {
      String guaranteeAttributeName = StringUtils.EMPTY;
      String guaranteeDurationAttributeName = StringUtils.EMPTY;
      for (ProductSpecialAttribute productSpecialAttribute : product.getProductSpecialAttributes()) {
        if (Objects.nonNull(productSpecialAttribute) && StringUtils.isNotEmpty(productSpecialAttribute.getAttributeName())) {
          if (GUARANTEE_ATTRIBUTE_NAME.contains(productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeAttributeName = productSpecialAttribute.getAttributeValue();
          } else if (GUARANTEE_DURATION_ATTRIBUTE_NAME.contains(productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeDurationAttributeName = productSpecialAttribute.getAttributeValue();
          }
        }
      }
      if (StringUtils.isNotEmpty(guaranteeAttributeName)) {
        if (StringUtils.isNotEmpty(guaranteeDurationAttributeName)) {
          StringBuilder warrantyInfo =
              new StringBuilder(guaranteeAttributeName).append(SPACE).append(guaranteeDurationAttributeName);
          itemDetail.setWarrantyInfo(warrantyInfo.toString());
        } else {
          itemDetail.setWarrantyInfo(guaranteeAttributeName);
        }
      }
    }
    itemDetail.setItemSku(item.getItemSku());
    itemDetail.setMerchantCode(product.getMerchantCode());
    itemDetail.setMerchantSku(item.getMerchantSku());
    itemDetail.setProductTypeCode(product.getProductType().getCode());
    itemDetail.setProductTypeName(product.getProductType().getDescription());
    itemDetail.setProductCode(product.getProductCode());
    itemDetail.setItemCatalogs(itemCatalogs);
    itemDetail.setProductCatentryId(product.getProductCatentryId());
    itemDetail.setItemCatentryId(item.getItemCatentryId());
    itemDetail.setProductSku(product.getProductSku());
    itemDetail.setSettlementType(product.getSettlementType());
    itemDetail.setTicketTemplateCode(item.getTicketTemplateCode());
    itemDetail.setInstallationRequired(product.isInstallationRequired());
    itemDetail.setSettlementType(ProductUtil.getSettlementType(product, item));
    itemDetail.setPickupPointCode(item.getPickupPointCode());
    itemDetail.setPreOrder(ObjectConverterUtil.checkPreOrder(product));
    itemDetail.setOff2OnChannelActive(item.isOff2OnChannelActive());
    boolean lateFulfillment = false;
    if (Boolean.TRUE.equals(item.isLateFulfillment())) {
      lateFulfillment = true;
    }
    itemDetail.setLateFulfillment(lateFulfillment);
    itemDetail.setWholesaleRules(item.getWholesaleRules());
    itemDetail.setOfferPrice(item.getPrice().stream().findFirst().get().getOfferPrice());
    itemDetail.setBuyable(item.getItemViewConfigs().stream().findFirst().orElse(new ItemViewConfig()).isBuyable() && !item.isArchived());
    itemDetail.setDiscoverable(
        item.getItemViewConfigs().stream().findFirst().orElse(new ItemViewConfig()).isDiscoverable() && !item.isArchived());
    itemDetail.setListPrice(item.getPrice().stream().findFirst().get().getListPrice());
  }

  @Override
  public List<SalesCatalog> convertToSalesCatalogs(List<ProductCategoryResponse> productCategoryResponses) {
    checkArgument(productCategoryResponses != null,
        ObjectConverterServiceImpl.PRODUCT_CATEGORY_RESPONSES_MUST_NOT_BE_NULL);

    List<String> salesCatalogCodes = new ArrayList<>();
    for (ProductCategoryResponse productCategoryResponse : productCategoryResponses) {
      String catalogCode = productCategoryResponse.getCategory().getCatalog().getCatalogCode();
      if (!ObjectConverterServiceImpl.CATALOG_CODE_MASTER.equals(catalogCode) && !salesCatalogCodes.contains(catalogCode)) {
        salesCatalogCodes.add(catalogCode);
      }
    }

    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    for (String salesCatalogCode : salesCatalogCodes) {
      List<CategoryResponse> categoryResponsesInCatalog = new ArrayList<>();
      for (ProductCategoryResponse productCategoryResponse : productCategoryResponses) {
        if (salesCatalogCode.equals(productCategoryResponse.getCategory().getCatalog().getCatalogCode())) {
          categoryResponsesInCatalog.add(productCategoryResponse.getCategory());
        }
      }

      List<CategoryResponse> rootCategoryCodes = new ArrayList<>();
      for (CategoryResponse categoryResponse : categoryResponsesInCatalog) {
        if (categoryResponse.getParentCategoryId() == null) {
          rootCategoryCodes.add(categoryResponse);
        }
      }

      List<String> lowestLevelCategoryCodes = new ArrayList<>();
      this.findLeaf(lowestLevelCategoryCodes, rootCategoryCodes, categoryResponsesInCatalog);
      List<Category> categoriesInACatalog = new ArrayList<>();
      for (String lowestLevelCategoryCode : lowestLevelCategoryCodes) {
        Category category = new Category();
        category.setCategoryCode(lowestLevelCategoryCode);
        category.setCatgroupId(lowestLevelCategoryCode);

        categoriesInACatalog.add(category);
      }
      SalesCatalog salesCatalog = new SalesCatalog();
      salesCatalog.setCatalogCode(salesCatalogCode);
      salesCatalog.setListOfCategories(categoriesInACatalog);

      salesCatalogs.add(salesCatalog);
    }

    return salesCatalogs;
  }

  @Override
  public List<SalesCatalog> convertToSalesCatalogsFromDirectParentCategory(String requestId, String username,
      List<CategoryReferenceResponse> categoryReferenceResponse,
      List<CategoryReferenceResponse> b2bSalesCategoryReferences, boolean umkmMerchant) {
    Map<String, List<Category>> salesCategoryMap = new HashMap<>();
    for (CategoryReferenceResponse salesCategoryReference : categoryReferenceResponse) {
      CategoryDetailResponse salesCategoryResponse =
          this.productCategoryBaseClient.getCategoryDetail(requestId, username,
              salesCategoryReference.getSalesCategoryReference().getId());
      salesCategoryMapping(salesCategoryResponse, salesCategoryMap, umkmMerchant);
    }
    for (CategoryReferenceResponse b2bSalesCategoryReference : b2bSalesCategoryReferences) {
      CategoryDetailResponse salesCategoryResponse =
          this.productCategoryBaseClient.getCategoryDetail(requestId, username,
              b2bSalesCategoryReference.getB2bSalesCategoryReference().getId());
      salesCategoryMapping(salesCategoryResponse, salesCategoryMap, umkmMerchant);
    }
    List<SalesCatalog> saleCatalogs = new ArrayList<>();
    for (String salesCatalogCode : salesCategoryMap.keySet()) {
      saleCatalogs.add(new SalesCatalog(salesCatalogCode, salesCategoryMap.get(salesCatalogCode)));
    }
    return saleCatalogs;
  }

  public void salesCategoryMapping(CategoryDetailResponse salesCategoryResponse,
      Map<String, List<Category>> salesCategoryMap, boolean umkmMerchant) {
      if (salesCategoryResponse == null) {
        return;
      }
      String salesCatalogCode = salesCategoryResponse.getCatalog().getCatalogCode();
      salesCategoryMap.computeIfAbsent(salesCatalogCode, k -> new ArrayList<>());
      if (salesCategoryResponse.isUmkm() && umkmMerchant) {
        salesCategoryMap.get(salesCatalogCode)
            .add(new Category(salesCategoryResponse.getCategoryCode(), salesCategoryResponse.getCategoryCode()));

      } else if (!salesCategoryResponse.isUmkm()) {
        salesCategoryMap.get(salesCatalogCode)
            .add(new Category(salesCategoryResponse.getCategoryCode(), salesCategoryResponse.getCategoryCode()));
      }

  }

  @Override
  public OfferedSummaryVo convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
      MasterDataProductAndItemsVO masterDataProductAndItemsVO, String itemCode, String itemSku) {
    checkArgument(masterDataProductAndItemsVO != null,
        ObjectConverterServiceImpl.MASTER_PRODUCT_AND_ITEMS_VO_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(itemCode), ObjectConverterServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);

    OfferedSummaryVo offeredSummaryVo = new OfferedSummaryVo();
    offeredSummaryVo.setId(itemCode);
    offeredSummaryVo.setItemSku(itemSku);
    offeredSummaryVo.setBrand(masterDataProductAndItemsVO.getMasterDataProduct().getBrand());
    offeredSummaryVo.setName(masterDataProductAndItemsVO.getMasterDataProduct().getProductName());
    MasterDataItem masterDataItem = masterDataProductAndItemsVO.getMasterDataItems().get(itemCode);
    offeredSummaryVo.setImageUrl(
        masterDataItem.getMasterDataItemImages().stream().filter(image -> BooleanUtils.isTrue(image.isMainImage())).findAny().get().getLocationPath());

    Map<String, String> attributes = masterDataItem.getMasterDataItemAttributeValues().stream().filter(attribute -> StringUtils.equals(attribute.getMasterDataAttribute().getAttributeType().name(),
        MasterDataAttributeType.DEFINING_ATTRIBUTE.name())).collect(
        Collectors.toMap(entry -> entry.getMasterDataAttribute().getAttributeName(), MasterDataItemAttributeValue::getAttributeValue,
            (oldAttributeValue, newAttributeValue) -> {
              return newAttributeValue;
            }));

    offeredSummaryVo.setAttributes(attributes);

    List<AttributeVo> offerPageHeaderSimilarAttributes = new ArrayList<>();

    masterDataProductAndItemsVO.getMasterDataItems().forEach((key, value) -> {

      if (!StringUtils.equals(itemCode, key)) {
        offerPageHeaderSimilarAttributes.add(new AttributeVo(value.getSkuCode(),
            value.getMasterDataItemAttributeValues().stream().filter(attribute -> StringUtils.equals(attribute.getMasterDataAttribute().getAttributeType().name(),
                    MasterDataAttributeType.DEFINING_ATTRIBUTE.name())).collect(Collectors.toMap(entry -> entry.getMasterDataAttribute().getAttributeName(),
                    MasterDataItemAttributeValue::getAttributeValue,
                    (oldAttributeValue, newAttributeValue) -> newAttributeValue))));
      }
    });

    offeredSummaryVo.setOtherAttributes(offerPageHeaderSimilarAttributes);

    return offeredSummaryVo;
  }

  @Override
  public OfferedSummaryVo convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
      PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo) {
    OfferedSummaryVo offeredSummaryVo = gdnMapper.deepCopy(pristineItemDetailAndMappingVo, OfferedSummaryVo.class);
    List<AttributeVo> attributeVos = new ArrayList<>();
    for (PristineSimilarItemVo pristineSimilarItemVo : pristineItemDetailAndMappingVo.getOtherPristineItems()) {
      attributeVos.add(new AttributeVo(pristineSimilarItemVo.getId(), pristineSimilarItemVo.getAttributes()));
    }
    offeredSummaryVo.setOtherAttributes(attributeVos);
    return offeredSummaryVo;
  }

  @Override
  public OfferedSummaryVo convertProductToOfferedSummaryVo(MandatoryRequestParam param, Product product, Item item)
      throws Exception {
    OfferedSummaryVo offeredSummaryVo = new OfferedSummaryVo();
    offeredSummaryVo.setId(item.getItemSku());
    offeredSummaryVo.setItemSku(item.getItemSku());
    offeredSummaryVo.setImageUrl(
        item.getMasterDataItem().getMasterDataItemImages().stream().filter(MasterDataItemImage::isMainImage).findFirst()
            .orElse(new MasterDataItemImage()).getLocationPath());
    offeredSummaryVo.setName(product.getMasterDataProduct().getProductName());
    offeredSummaryVo.setBrand(product.getMasterDataProduct().getBrand());
    offeredSummaryVo.setStoreId(product.getStoreId());

    Map<String, String> attributes = product.getDefiningAttributes().stream().filter(productAttribute -> productAttribute.getItemSku().equalsIgnoreCase(item.getItemSku())).findFirst()
        .orElse(new ProductAttribute()).getProductAttributeDetails().stream().collect(
            Collectors.toMap(ProductAttributeDetail::getAttributeName, ProductAttributeDetail::getAttributeValue,
                (oldAttributeValue, newAttributeValue) -> newAttributeValue));

    offeredSummaryVo.setAttributes(attributes);

    List<AttributeVo> offerPageHeaderSimilarAttributes = new ArrayList<>();
    product.getDefiningAttributes().stream().filter(productAttribute -> !productAttribute.getItemSku().equalsIgnoreCase(item.getItemSku()))
        .forEach(productAttribute -> {
          Map<String, String> otherAttributes = productAttribute.getProductAttributeDetails().stream().collect(
              Collectors.toMap(ProductAttributeDetail::getAttributeName, ProductAttributeDetail::getAttributeValue, (oldAttributeValue, newAttributeValue) -> newAttributeValue));

          AttributeVo attributeVo = new AttributeVo();
          attributeVo.setId(productAttribute.getItemSku());
          attributeVo.setAttributes(otherAttributes);
          offerPageHeaderSimilarAttributes.add(attributeVo);
        });
    offeredSummaryVo.setOtherAttributes(offerPageHeaderSimilarAttributes);
    return offeredSummaryVo;
  }

  private void findLeaf(List<String> lowestLevelCategoryCodes, List<CategoryResponse> parentCategories, List<CategoryResponse> categoryList) {
    categoryList.removeAll(parentCategories);
    List<CategoryResponse> nextParentCategories = new ArrayList<>();
    for (CategoryResponse parentCategory : parentCategories) {
      boolean hasChild = false;
      for (CategoryResponse category : categoryList) {
        if (category.getParentCategoryId().equals(parentCategory.getId())) {
          nextParentCategories.add(category);
          hasChild = true;
        }
      }
      if (!hasChild) {
        lowestLevelCategoryCodes.add(parentCategory.getCategoryCode());
      }
    }
    if (nextParentCategories.size() != 0) {
      this.findLeaf(lowestLevelCategoryCodes, nextParentCategories, categoryList);
    }
  }

  @Override
  public List<ComboItemVO> convertComboRulesResponseToComboItemVO(List<ComboRuleVO> comboRuleVOs) {
    List<ComboItemVO> comboItemVOs = new ArrayList<>();

    comboRuleVOs.stream().forEach(comboRule -> {
      comboItemVOs.add(convertToComboItemVO(comboRule));
    });

    return comboItemVOs;
  }

  private ComboItemVO convertToComboItemVO(ComboRuleVO comboRuleVO) {
    ComboItemVO comboItemVO = new ComboItemVO();
    comboItemVO.setItemSku(comboRuleVO.getItemSku());
    comboItemVO.setDiscountPercentage(comboRuleVO.getDiscountPercentage());
    comboItemVO.setMainSku(comboRuleVO.isMainSku());
    comboItemVO.setQuantity(comboRuleVO.getQuantity());

    return comboItemVO;
  }

  @Override
  public void convertItemAndProductToComboItemVO(ComboItemVO comboItemVO, Item item, Product product) {
    comboItemVO.setProductName(product.getMasterDataProduct().getProductName());
    if (item.getPristineDataItem() != null) {
      comboItemVO.setProductName(item.getPristineDataItem().getPristineProductName());
      comboItemVO.setPristineId(item.getPristineDataItem().getPristineId());
    }
    comboItemVO.setMasterDataItemAttributeValues(convertMasterDataItemAttributeToMasterDataItemAttributeVO(
        item.getMasterDataItem().getMasterDataItemAttributeValues()));
    comboItemVO.setBuyable(item.getItemViewConfigs().stream().findFirst().orElse(new ItemViewConfig()).isBuyable());
    comboItemVO.setItemCode(item.getItemCode());
    comboItemVO.setImageUrl(item.getMasterDataItem().getMasterDataItemImages().stream()
        .filter(masterDataItemImage -> masterDataItemImage.isMainImage()).findFirst().orElse(new MasterDataItemImage()).getLocationPath());
    Price price = item.getPrice().stream().findFirst().orElse(new Price());
    comboItemVO.setOfferPrice(price.getOfferPrice());
    comboItemVO.setListPrice(price.getListPrice());
    comboItemVO.setCurrency(price.getCurrency());
    comboItemVO.setProductAdjustmentPrice(itemHelperService.getDiscountPrice(price));
    comboItemVO.setProductSku(item.getProductSku());
  }

  @Override
  public void convertItemAndProductToItemInfoVO(ItemInfoVO itemInfoVO, Item item, Product product) {
    itemInfoVO.setProductName(product.getMasterDataProduct().getProductName());
    if (Objects.nonNull(item.getPristineDataItem())) {
      PristineDataItem pristineDataItem = item.getPristineDataItem();
      itemInfoVO.setPristineId(pristineDataItem.getPristineId());
      if (StringUtils.isBlank(pristineDataItem.getPristineModel()) && StringUtils.isBlank(pristineDataItem.getPristineBrand())) {
        if (Objects.nonNull(item.getMasterDataItem())) {
          itemInfoVO.setProductName(item.getMasterDataItem().getGeneratedItemName());
        }
      } else {
        itemInfoVO.setProductName(item.getPristineDataItem().getPristineProductName());
      }
    }
    itemInfoVO.setItemSku(item.getItemSku());
    itemInfoVO.setBuyable(item.getItemViewConfigs().stream().findFirst().orElse(new ItemViewConfig()).isBuyable());
    itemInfoVO.setItemCode(item.getItemCode());
    if (Objects.nonNull(item.getMasterDataItem())) {
      if (CollectionUtils.isNotEmpty(item.getMasterDataItem().getMasterDataItemImages())) {
        itemInfoVO.setImageUrl(item.getMasterDataItem().getMasterDataItemImages().stream()
            .filter(masterDataItemImage -> masterDataItemImage.isMainImage()).findFirst()
            .orElse(new MasterDataItemImage()).getLocationPath());
      }
      itemInfoVO.setMasterDataItemAttributes(convertMasterDataItemAttributeToMasterDataItemAttributeVO(
          item.getMasterDataItem().getMasterDataItemAttributeValues()));
    }
    Price price = item.getPrice().stream().findFirst().orElse(new Price());
    itemInfoVO.setOfferPrice(price.getOfferPrice());
    itemInfoVO.setListPrice(price.getListPrice());
    itemInfoVO.setCurrency(price.getCurrency());
    itemInfoVO.setProductAdjustmentPrice(itemHelperService.getDiscountPrice(price));
    itemInfoVO.setProductSku(item.getProductSku());
    List<OfflineItemDetailVo> offlineItems = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList =
        this.itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(item.getStoreId(), item.getItemSku(), Boolean.TRUE,
            Boolean.FALSE);
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      OfflineItemDetailVo offlineItemDetailVo = new OfflineItemDetailVo();
      offlineItemDetailVo.setUniqueId(itemPickupPoint.getOfflineItemId());
      offlineItemDetailVo.setItemSku(itemPickupPoint.getItemSku());
      offlineItemDetailVo.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
      offlineItemDetailVo.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      offlineItemDetailVo.setPrices(itemPickupPoint.getPrice());
      offlineItems.add(offlineItemDetailVo);
    }
    itemInfoVO.setOfflineItems(offlineItems);
    if (CollectionUtils.isNotEmpty(item.getWholesaleRules())) {
      itemInfoVO.setWholesaleRules(item.getWholesaleRules());
    }
  }

  private List<MasterDataItemAttributeVO> convertMasterDataItemAttributeToMasterDataItemAttributeVO(
      List<MasterDataItemAttributeValue> masterDataItemAttributeValue) {

    List<MasterDataItemAttributeVO> masterDataItemAttributes = new ArrayList<>();
    masterDataItemAttributeValue.stream().filter(masterDataItemAttributeValue1 -> StringUtils.equals(masterDataItemAttributeValue1.getMasterDataAttribute().getAttributeType().name(),
            MasterDataAttributeType.DEFINING_ATTRIBUTE.name()) || masterDataItemAttributeValue1.getMasterDataAttribute()
            .isVariantCreation()).forEach(masterDataAttributeValue -> {
      MasterDataItemAttributeVO masterDataItemAttributeVO = new MasterDataItemAttributeVO();
      masterDataItemAttributeVO.setAttributeValue(masterDataAttributeValue.getAttributeValue());
      masterDataItemAttributeVO.setAttributeCode(masterDataAttributeValue.getMasterDataAttribute().getAttributeCode());
      masterDataItemAttributeVO.setAttributeName(masterDataAttributeValue.getMasterDataAttribute().getAttributeName());
      masterDataItemAttributes.add(masterDataItemAttributeVO);
    });

    return masterDataItemAttributes;
  }

  private List<MasterDataItemAttributeVO> convertProductAttributeDetailToMasterDataItemAttributeVO(
      List<ProductAttributeDetail> productAttributeDetails) {
    List<MasterDataItemAttributeVO> masterDataItemAttributes = new ArrayList<>();
    if (CollectionUtils.isEmpty(productAttributeDetails)) {
      return masterDataItemAttributes;
    }
    productAttributeDetails.stream().forEach(productAttributeDetail -> {
      MasterDataItemAttributeVO masterDataItemAttributeVO = new MasterDataItemAttributeVO();
      masterDataItemAttributeVO.setAttributeValue(productAttributeDetail.getAttributeValue());
      masterDataItemAttributeVO.setAttributeCode(productAttributeDetail.getAttributeCode());
      masterDataItemAttributeVO.setAttributeName(productAttributeDetail.getAttributeName());
      masterDataItemAttributes.add(masterDataItemAttributeVO);
    });

    return masterDataItemAttributes;
  }

  @Override
  public ComboVO convertPromoBundlingDetailResponseVoToComboVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO) {

    ComboVO comboVO = new ComboVO();
    comboVO.setPromoBundlingId(promoBundlingDetailResponseVO.getPromoBundlingId());
    comboVO.setPromoBundlingName(promoBundlingDetailResponseVO.getPromoBundlingName());
    comboVO.setPromoBundlingType(promoBundlingDetailResponseVO.getPromoBundlingType());
    comboVO.setStartDate(promoBundlingDetailResponseVO.getStartDate());
    comboVO.setEndDate(promoBundlingDetailResponseVO.getEndDate());

    return comboVO;
  }

  @Override
  public void convertProductToComboItemVO(ComboItemVO comboItemVO, Product product) {
    comboItemVO.setProductCode(product.getProductCode());
    comboItemVO.setMerchantCode(product.getMerchantCode());
    comboItemVO.setBrand(product.getMasterDataProduct().getBrand());


    Map<String, ItemCatalogVO> mapOfItemCatalogVo =
        product.getItemCatalogs().stream().collect(Collectors.toMap(ItemCatalogVO::getCatalogId, Function.identity(), (oldValue, newValue) -> oldValue));

    if (!(product.getMasterDataProduct().getMasterCatalog() == null)) {
      comboItemVO.setMasterCatalog(mapOfItemCatalogVo.get(product.getMasterDataProduct().getMasterCatalog().getCatalogCode()));
    }
    comboItemVO.setSalesCatalogs(constructDetailSalesCatalog(product, mapOfItemCatalogVo));
  }

  @Override
  public void convertProductToItemInfoVO(ItemInfoVO itemInfoVO, Product product) {
    itemInfoVO.setBrand(product.getMasterDataProduct().getBrand());
    itemInfoVO.setProductCode(product.getProductCode());
    itemInfoVO.setMerchantCode(product.getMerchantCode());

    Map<String, ItemCatalogVO> mapOfItemCatalogVo =
        product.getItemCatalogs().stream().collect(Collectors.toMap(ItemCatalogVO::getCatalogId, Function.identity(), (oldValue, newValue) -> oldValue));

    if (!(product.getMasterDataProduct().getMasterCatalog() == null)) {
      itemInfoVO.setMasterCatalog(mapOfItemCatalogVo.get(product.getMasterDataProduct().getMasterCatalog().getCatalogCode()));
    }
    itemInfoVO.setSalesCatalogs(constructDetailSalesCatalog(product, mapOfItemCatalogVo));
  }

  private List<ItemCatalogVO> constructDetailSalesCatalog(Product product,
      Map<String, ItemCatalogVO> mapOfItemCatalogVo) {
    List<ItemCatalogVO> salesCatalogs = new ArrayList<>();

    product.getSalesCatalogs().stream().forEach(salesCatalog -> {
      ItemCatalogVO itemCatalogVO = mapOfItemCatalogVo.get(salesCatalog.getCatalogCode());
      if (itemCatalogVO != null) {
        salesCatalogs.add(itemCatalogVO);
      }
    });
    return salesCatalogs;
  }

  @Override
  public List<PromoBundlingDetailResponseVO> convertPromoBundlingDetailResponseToPromoBundlingDetailVo(List<PromoBundlingDetailResponse> promoBundlingDetailResponses) {
    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOs = new ArrayList<>();
    promoBundlingDetailResponses.stream().forEachOrdered(promoBundlingDetailResponse -> {
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO =
          gdnMapper.deepCopy(promoBundlingDetailResponse, PromoBundlingDetailResponseVO.class);
      promoBundlingDetailResponseVOs.add(promoBundlingDetailResponseVO);
    });

    return promoBundlingDetailResponseVOs;
  }

  @Override
  public WholesaleVO convertPromoBundlingDetailResponseToWholesaleVO(
      PromoBundlingByItemSkuAndItemCodesResponseVO promoBundlingByItemSkuAndItemCodesResponseVO, Item item) {
    WholesaleVO wholesaleVO = new WholesaleVO();
    wholesaleVO.setTotal(promoBundlingByItemSkuAndItemCodesResponseVO.getTotalWholesaleRule());
    wholesaleVO.setItemCode(item.getItemCode());
    wholesaleVO.setItemSku(item.getItemSku());
    Price price = item.getPrice().stream().findFirst().orElse(new Price());
    wholesaleVO.setOfferPrice(price.getOfferPrice());
    wholesaleVO.setListPrice(price.getListPrice());
    wholesaleVO.setProductAdjustmentPrice(itemHelperService.getDiscountPrice(price));
    return wholesaleVO;
  }

  @Override
  public List<WholesaleRuleVO> convertPromoBundlingDetailResponseToWholesaleRuleVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO) {
    List<WholesaleRuleVO> wholesaleRuleVOList = new ArrayList<>();

    if (CollectionUtils.isNotEmpty(promoBundlingDetailResponseVO.getWholesaleRules())) {
      promoBundlingDetailResponseVO.getWholesaleRules().forEach(wholesaleRule -> {
        WholesaleRuleVO wholesaleRuleVO = new WholesaleRuleVO();
        wholesaleRuleVO.setDiscountPercentage(wholesaleRule.getDiscountPercentage());
        wholesaleRuleVO.setMinQuantity(wholesaleRule.getMinQuantity());
        wholesaleRuleVO.setMaxQuantity(wholesaleRule.getMaxQuantity());

        wholesaleRuleVOList.add(wholesaleRuleVO);
      });
    }

    return wholesaleRuleVOList;
  }

  @Override
  public PromoBundlingVO convertPromoBundlingDetailResponseVOToPromoBundlingVO(
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO, Map<String, Price> priceByItemSku) {
    PromoBundlingVO promoBundlingVO = new PromoBundlingVO();
    promoBundlingVO.setPromoBundlingId(promoBundlingDetailResponseVO.getPromoBundlingId());
    promoBundlingVO.setPromoBundlingType(promoBundlingDetailResponseVO.getPromoBundlingType());
    promoBundlingVO.setPromoBundlingName(promoBundlingDetailResponseVO.getPromoBundlingName());
    promoBundlingVO.setStartDate(promoBundlingDetailResponseVO.getStartDate());
    promoBundlingVO.setEndDate(promoBundlingDetailResponseVO.getEndDate());
    promoBundlingVO.setComboRules(convertComboRuleVOToPromoBundlingVO(promoBundlingDetailResponseVO.getComboRules(), priceByItemSku));

    return promoBundlingVO;
  }

  private List<ComboRuleVO> convertComboRuleVOToPromoBundlingVO(List<ComboRuleVO> comboRuleVOList, Map<String, Price> priceByItemSku) {
    List<ComboRuleVO> comboRules = new ArrayList<>();
    comboRuleVOList.stream().forEach(comboRuleVO -> {
      ComboRuleVO comboRule = new ComboRuleVO();
      comboRule.setItemSku(comboRuleVO.getItemSku());
      comboRule.setDiscountPercentage(comboRuleVO.getDiscountPercentage());
      double finalPrice = this.itemPriceService.getFinalPrice(priceByItemSku.get(comboRuleVO.getItemSku()), comboRuleVO.getDiscountPercentage());
      comboRule.setFinalPrice(finalPrice);
      comboRule.setQuantity(comboRuleVO.getQuantity());
      comboRule.setMainSku(comboRuleVO.isMainSku());
      comboRules.add(comboRule);
    });

    return comboRules;
  }

  @Override
  public MandatoryRequestParam generateMandatoryRequestParam(BusinessPartnerChange businessPartnerChange)
      throws Exception {
    return ObjectConverterUtil.generateMandatoryRequestParam(businessPartnerChange);
  }

  @Override
  public boolean getItemForActivation(Item item, NeedCorrectionItemActivationRequest itemActivationRequest,
      ItemPickupPoint itemPickupPoint) {
    boolean isShippingChanged = false;
    for (ItemViewConfig itemViewConfig : item.getItemViewConfigs()) {
      itemViewConfig.setBuyable(itemActivationRequest.isBuyable());
      itemViewConfig.setDiscoverable(itemActivationRequest.isDiscoverable());
    }
    for (Price price : item.getPrice()) {
      price.setListPrice(itemActivationRequest.getListPrice());
      price.setOfferPrice(itemActivationRequest.getOfferPrice());
    }
    Boolean wholesalePriceActivated = itemActivationRequest.getWholesalePriceActivated();
    if (Objects.nonNull(wholesalePriceActivated)) {
      item.setWholesalePriceExists(true);
    }
    if (Objects.isNull(wholesalePriceActivated) || !wholesalePriceActivated) {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings())) {
        item.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
    } else {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings())) {
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        item.setActivePromoBundlings(new HashSet<>());
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      }
    }
    item.setMerchantSku(itemActivationRequest.getMerchantSku());
    item.setMarkForDelete(false);
    item.setForceReview(false);
    item.setArchived(item.isArchivedBeforeEdit());
    if (StringUtils.isNotBlank(itemActivationRequest.getPickupPointCode()) && !StringUtils.equals(item.getPickupPointCode(), itemActivationRequest.getPickupPointCode())) {
      item.setPickupPointCode(itemActivationRequest.getPickupPointCode());
      LOGGER.info("PickupPointCode changed for itemSku: {}", item.getItemSku());
      isShippingChanged = true;
    }
    overrideL5DetailsFromL4ExcludePickupPointCode(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    return isShippingChanged;
  }

  private void overrideL5DetailsFromL4ExcludePickupPointCode(List<Item> items, List<ItemPickupPoint> itemPickupPoints) {
    Map<String, ItemPickupPoint> itemSkuAndItemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    for (Item item : items) {
      if (itemSkuAndItemPickupPointMap.containsKey(item.getItemSku())) {
        ItemPickupPoint itemPickupPoint = itemSkuAndItemPickupPointMap.get(item.getItemSku());
        itemPickupPoint.setItemViewConfig(item.getItemViewConfigs());
        itemPickupPoint.setPrice(item.getPrice());
        itemPickupPoint.setWholesalePriceExists(item.isWholesalePriceExists());
        itemPickupPoint.setPromoBundling(item.isPromoBundling());
        itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
        itemPickupPoint.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
        itemPickupPoint.setFlashSaleActive(item.isFlashSaleActive());
      }
    }
  }

  @Override
  public ItemPickupPoint updateItemPickupPointOnNeedCorrectionActivation(NeedCorrectionItemActivationRequest itemActivationRequest,
      ItemPickupPoint itemPickupPoint, BusinessPartnerPickupPoint businessPartnerPickupPoint) {
    if (!businessPartnerPickupPoint.isCncActivated()) {
      itemActivationRequest.setCncBuyable(false);
      itemActivationRequest.setCncDiscoverable(false);
    }
    if (!businessPartnerPickupPoint.isDelivery()) {
      itemActivationRequest.setBuyable(false);
      itemActivationRequest.setDiscoverable(false);
    }
    if (CollectionUtils.isEmpty(itemPickupPoint.getItemViewConfig())) {
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setChannel(Constants.DEFAULT);
      itemViewConfig.setBuyable(itemActivationRequest.isBuyable());
      itemViewConfig.setDiscoverable(itemActivationRequest.isDiscoverable());
      itemPickupPoint.getAllItemViewConfigs().add(itemViewConfig);
      ItemViewConfig cncItemViewConfig = new ItemViewConfig();
      cncItemViewConfig.setChannel(Constants.CNC);
      cncItemViewConfig.setBuyable(itemActivationRequest.isCncBuyable());
      cncItemViewConfig.setDiscoverable(itemActivationRequest.isCncDiscoverable());
      itemPickupPoint.getAllItemViewConfigs().add(cncItemViewConfig);
      if (Objects.nonNull(itemActivationRequest.getB2bFields())) {
        ItemViewConfig itemViewConfigB2b = new ItemViewConfig();
        itemViewConfigB2b.setChannel(Constants.B2B);
        Set<ItemViewConfig> itemViewConfigB2bRequest =
            Optional.ofNullable(itemActivationRequest.getB2bFields().getB2bItemViewConfigs()).orElse(new HashSet<>());
        itemViewConfigB2b.setBuyable(itemViewConfigB2bRequest.iterator().next().isBuyable());
        itemViewConfigB2b.setDiscoverable(itemViewConfigB2bRequest.iterator().next().isDiscoverable());
        itemPickupPoint.getAllItemViewConfigs().add(itemViewConfigB2b);
      }
    } else {
      for (ItemViewConfig itemViewConfig : itemPickupPoint.getItemViewConfig()) {
        itemViewConfig.setBuyable(itemActivationRequest.isBuyable());
        itemViewConfig.setDiscoverable(itemActivationRequest.isDiscoverable());
      }
      ItemViewConfig cncItemViewConfig =
          itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(ChannelName.CNC.name());
      cncItemViewConfig.setChannel(Constants.CNC);
      cncItemViewConfig.setDiscoverable(itemActivationRequest.isCncDiscoverable());
      cncItemViewConfig.setBuyable(itemActivationRequest.isCncBuyable());
      itemPickupPoint.getAllItemViewConfigs().add(cncItemViewConfig);
      if (Objects.nonNull(itemActivationRequest.getB2bFields())) {
        ItemViewConfig itemViewConfigB2b = itemPickupPoint.getAllItemViewConfigs().stream()
            .filter(itemViewConfig -> itemViewConfig.getChannel().equalsIgnoreCase(Constants.B2B)).findFirst()
            .orElse(new ItemViewConfig());
        Set<ItemViewConfig> itemViewConfigB2bRequest =
            Optional.ofNullable(itemActivationRequest.getB2bFields().getB2bItemViewConfigs()).orElse(new HashSet<>());
        itemViewConfigB2b.setChannel(Constants.B2B);
        if (CollectionUtils.isNotEmpty(itemViewConfigB2bRequest)) {
          itemViewConfigB2b.setBuyable(itemViewConfigB2bRequest.iterator().next().isBuyable());
          itemViewConfigB2b.setDiscoverable(itemViewConfigB2bRequest.iterator().next().isDiscoverable());
          itemPickupPoint.getAllItemViewConfigs().add(itemViewConfigB2b);
        }
      }
    }
    if (CollectionUtils.isEmpty(itemPickupPoint.getPrice())) {
      Price price = new Price();
      price.setChannel(Constants.DEFAULT);
      price.setOfferPrice(itemActivationRequest.getOfferPrice());
      price.setListPrice(itemActivationRequest.getListPrice());
      itemPickupPoint.getPrice().add(price);
    } else {
      for (Price price : itemPickupPoint.getPrice()) {
        price.setListPrice(itemActivationRequest.getListPrice());
        price.setOfferPrice(itemActivationRequest.getOfferPrice());
      }
    }
    Boolean wholesalePriceActivated = itemActivationRequest.getWholesalePriceActivated();
    if (Objects.nonNull(wholesalePriceActivated)) {
      itemPickupPoint.setWholesalePriceExists(true);
    }
    if (Objects.isNull(wholesalePriceActivated) || !wholesalePriceActivated) {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
        itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
    } else {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
        itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        itemPickupPoint.setActivePromoBundlings(new HashSet<>());
        itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      }
    }
    if (!cncForWarehouseFeatureSwitch) {
      itemPickupPoint.setCncActive(itemActivationRequest.isCncActive());
    }
    itemPickupPoint.setFbbActivated(itemActivationRequest.isFbbActivated());
    itemPickupPoint.setMerchantSku(itemActivationRequest.getMerchantSku());
    itemPickupPoint.setDistribution(itemActivationRequest.isDistribution());
    itemPickupPoint.setMarkForDelete(false);
    if (Objects.nonNull(itemActivationRequest.getB2bFields())) {
      B2bFields b2bFields = new B2bFields(itemActivationRequest.getB2bFields().isManaged(),
          itemActivationRequest.getB2bFields().getBasePrice());
      itemPickupPoint.setB2bFields(b2bFields);
    }
    return itemPickupPoint;
  }

  @Override
  public Product convertToProduct(ProductEventModel productEventModel) {
    Product product = new Product();
    BeanUtils.copyProperties(productEventModel, product, "masterDataProduct");
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(productEventModel.getPreOrder());
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(productEventModel.getCatalogCode());
    if (Objects.nonNull(productEventModel.getCategoryCode())) {
      masterCatalog.setCategory(
        new Category(productEventModel.getCategoryCode(), productEventModel.getCategoryCode()));
    }
    masterDataProduct.setMasterCatalog(masterCatalog);
    masterDataProduct.setBrand(productEventModel.getBrand());
    masterDataProduct.setProductName(productEventModel.getProductName());
    product.setPreOrder(preOrder);
    product.setMasterDataProduct(masterDataProduct);
    product.setProductScore(toProductScore(productEventModel.getProductScore()));
    product.setSalesCatalogs(convertToSalesCatalogsFromSalesCatalogsModel(productEventModel.getSalesCatalogs()));
    product.setCurationStatus(CurationStatus.fromValue(productEventModel.getCurationStatus()));
    product.setDistributionStatus(DistributionStatus.getByCode(productEventModel.getDistributionStatus()));
    return product;
  }

  private List<SalesCatalog> convertToSalesCatalogsFromSalesCatalogsModel(List<SalesCatalogModel> salesCatalogModels) {
    return ObjectConverterUtil.convertToSalesCatalogsFromSalesCatalogsModel(salesCatalogModels);
  }

  private Category toCategoryList(String categoryCode) {
    return ObjectConverterUtil.toCategoryList(categoryCode);
  }

  @Override
  public ProductScore toProductScore(ProductScoreVo productScoreVo) {
    ProductScore productScore = new ProductScore();
    BeanUtils.copyProperties(productScoreVo, productScore);
    return productScore;
  }

  @Override
  public List<Item> convertToListItem(List<ItemEventModel> itemEventModelList, String productSku,
      boolean needToOverrideL4DetailsFromL5) {
    List<Item> items = itemEventModelList.stream().map(itemEventModel -> convertToList(itemEventModel))
        .collect(Collectors.toList());
    if (needToOverrideL4DetailsFromL5) {
      List<ItemPickupPoint> itemPickupPoints = itemPickupPointService
          .findByStoreIdAndProductSkuAndDelivery(items.get(0).getStoreId(), productSku, true);
      this.overrideL4DetailsFromL5(items, itemPickupPoints);
    }
    return items;
  }

  private Item convertToList(ItemEventModel itemEventModel) {
    Item item = new Item();
    BeanUtils.copyProperties(itemEventModel, item);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(itemEventModel.getPristineId());
    item.setPristineDataItem(pristineDataItem);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(itemEventModel.getGeneratedItemName());
    item.setMasterDataItem(masterDataItem);
    ItemViewConfig itemViewConfig =
        new ItemViewConfig(itemEventModel.isBuyable(), itemEventModel.isDiscoverable(), itemEventModel.getChannel(),
            null, null);
    item.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfig)));
    item.setPrice(toPriceEntity(itemEventModel.getPrice()));
    return item;
  }

  private Set<Price> toPriceEntity(Set<PriceModel> priceModelSet) {
    Set<Price> prices = new HashSet<>();
    for (PriceModel priceModel : priceModelSet) {
      Price price = new Price();
      price.setOfferPrice(priceModel.getOfferPrice());
      price.setListPrice(priceModel.getListPrice());
      price.setChannel(priceModel.getChannel());
      price.setCurrency(priceModel.getCurrency());
      if (Objects.nonNull(priceModel.getMerchantPromoDiscountPrice())) {
        price.setMerchantPromoDiscountPrice(toMerchantPromoDiscountPrice(priceModel.getMerchantPromoDiscountPrice()));
      }
      if (CollectionUtils.isNotEmpty(priceModel.getListOfDiscountPrices())) {
        price.setListOfDiscountPrices(priceModel.getListOfDiscountPrices().stream()
            .map(discountPrice -> toMerchantPromoDiscountPrice(discountPrice)).collect(toList()));
      }
      prices.add(price);
    }
    return prices;
  }

  private DiscountPrice toMerchantPromoDiscountPrice(DiscountPriceModel merchantPromoDiscountPrice) {
    return ObjectConverterUtil.toMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
  }

  private List<SalesCatalogModel> convertToSalesCatalogModel(List<SalesCatalog> salesCatalogs) {
    return salesCatalogs.stream().map(salesCatalog -> new SalesCatalogModel(salesCatalog.getCatalogCode(),
        salesCatalog.getListOfCategories().stream().map(category -> category.getCategoryCode()).collect(Collectors.toList()))).collect(Collectors.toList());
  }

  @Override
  public ProductEventModel convertToProductEventModel(Product product) {
    ProductEventModel productEventModel = new ProductEventModel();
    BeanUtils.copyProperties(product, productEventModel);
    productEventModel.setProductType(product.getProductType());
    productEventModel.setCurationStatus(CommonUtil.getCurationStatus(product));
    productEventModel.setBundleProduct(product.isBundleProduct());
    productEventModel.setMerchantCode(product.getMerchantCode());
    ProductScoreVo productScoreVo = new ProductScoreVo();
    BeanUtils.copyProperties(product.getProductScore(), productScoreVo);
    productEventModel.setProductScore(productScoreVo);
    if (Objects.nonNull(product.getMasterDataProduct())) {
      if (Objects.nonNull(product.getMasterDataProduct().getMasterCatalog())) {
        productEventModel.setCatalogCode(product.getMasterDataProduct().getMasterCatalog().getCatalogCode());
        productEventModel.setCategoryCode(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
      }
      productEventModel.setBrand(product.getMasterDataProduct().getBrand());
      productEventModel.setProductName(product.getMasterDataProduct().getProductName());
    }
    productEventModel.setSalesCatalogs(convertToSalesCatalogModel(product.getAllSalesCatalogs()));
    if (Objects.nonNull(product.getPreOrder())) {
      productEventModel.setPreOrder(product.getPreOrder().getIsPreOrder());
    }
    if (Objects.nonNull(product.getCurationStatus())) {
      productEventModel.setCurationStatus(product.getCurationStatus().getValue());
    }
    productEventModel.setSizeChartCode(product.getSizeChartCode());
    productEventModel.setDistributionStatus(
        Optional.ofNullable(product.getDistributionStatus()).orElse(DistributionStatus.NON_DISTRIBUTION).getCode());
    return productEventModel;
  }

  private DiscountPriceModel convertToDiscountPriceModel(DiscountPrice discountPrice) {
    return ObjectConverterUtil.convertToDiscountPriceModel(discountPrice);
  }

  private Set<PriceModel> convertToListPriceModel(Set<Price> prices) {
    Set<PriceModel> priceModels = new HashSet<>();
    for (Price price : prices) {
      PriceModel priceModel = new PriceModel();
      priceModel.setCurrency(price.getCurrency());
      priceModel.setChannel(price.getChannel());
      priceModel.setOfferPrice(price.getOfferPrice());
      priceModel.setListPrice(price.getListPrice());
      priceModel.setLastUpdatedBy(price.getLastUpdatedBy());
      priceModel.setLastUpdatedDate(price.getLastUpdatedDate());
      if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
        DiscountPrice discountPriceFinal =
          itemHelperService.processDiscountPricesByPriority(price.getListOfDiscountPrices());
        if(Objects.nonNull(discountPriceFinal)) {
          priceModel.setListOfDiscountPrices(new ArrayList<>(Arrays.asList(convertToDiscountPriceModel(discountPriceFinal))));
        }
        else {
          priceModel.setListOfDiscountPrices(Collections.emptyList());
        }
      }
      if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
        priceModel.setMerchantPromoDiscountPrice(convertToDiscountPriceModel(price.getMerchantPromoDiscountPrice()));
      }
      priceModels.add(priceModel);
    }
    return priceModels;
  }

  @Override
  public ItemEventModel convertToItemEventModel(Item item) {
    ItemEventModel itemEventModel = new ItemEventModel();
    BeanUtils.copyProperties(item, itemEventModel);
    itemEventModel.setBuyable(
        Optional.ofNullable(item.getItemViewConfigs()).orElse(new HashSet<>()).stream().findFirst()
            .orElse(new ItemViewConfig()).isBuyable());
    itemEventModel.setDiscoverable(
        Optional.ofNullable(item.getItemViewConfigs()).orElse(new HashSet<>()).stream().findFirst()
            .orElse(new ItemViewConfig()).isDiscoverable());
    itemEventModel.setChannel(
        Optional.ofNullable(item.getItemViewConfigs()).orElse(new HashSet<>()).stream().findFirst()
            .orElse(new ItemViewConfig()).getChannel());
    itemEventModel.setActivePromoBundlings(Optional.ofNullable(item.getActivePromoBundlings()).orElse(new HashSet<>()));
    itemEventModel.setCreatedDate(new Date());
    itemEventModel.setUpdatedDate(new Date());
    if (Objects.nonNull(item.getMasterDataItem()) && !item.isSynchronized()) {
      itemEventModel.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
    }
    if (Objects.nonNull(item.getPristineDataItem())) {
      itemEventModel.setPristineId(item.getPristineDataItem().getPristineId());
    }
    itemEventModel.setPrice(convertToListPriceModel(Optional.ofNullable(item.getPrice()).orElse(new HashSet<>())));
    itemEventModel.setMerchantCode(item.getMerchantCode());
    return itemEventModel;
  }

  @Override
  public List<ItemEventModel> convertToListItemEventModel(List<Item> items) {
    return items.stream().map(item -> convertToItemEventModel(item)).collect(Collectors.toList());
  }

  @Override
  public ProductAndItemEventModel convertToProductAndItemEventModel(ProductAndItemsVO productAndItemsVo) {
    return new ProductAndItemEventModel(productAndItemsVo.getProduct().getProductSku(),
      convertToProductEventModel(productAndItemsVo.getProduct()),
      convertToListItemEventModel(productAndItemsVo.getItems()), false, false,
      productAndItemsVo.getProduct().getMerchantCode());
  }

  @Override
  public ProductAndItemEventModel convertToProductAndItemEventModel(ProductAndItemsVO productAndItemsVo,
      List<ItemVo> itemVos) {
    return new ProductAndItemEventModel(productAndItemsVo.getProduct().getProductSku(),
        convertToProductEventModel(productAndItemsVo.getProduct()),
        convertToListItemEventModelFromItemPickupPoint(itemVos), false, false,
      productAndItemsVo.getProduct().getMerchantCode());
  }

  private List<ItemEventModel> convertToListItemEventModelFromItemPickupPoint(List<ItemVo> itemVos) {
    List<ItemEventModel> itemEventModelList = new ArrayList<>();
    for (ItemVo itemVo : itemVos) {
      for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
        ItemEventModel itemEventModel = convertToItemEventModel(itemVo);
        itemEventModel.setBuyable(itemPickupPointVo.getItemViewConfig().iterator().next().isBuyable());
        itemEventModel.setDiscoverable(itemPickupPointVo.getItemViewConfig().iterator().next().isDiscoverable());
        itemEventModel.setChannel(itemPickupPointVo.getItemViewConfig().iterator().next().getChannel());
        itemEventModel.setPrice(convertToListPriceModel(itemPickupPointVo.getPrice()));
        itemEventModel.setActivePromoBundlings(itemPickupPointVo.getActivePromoBundlings());
        itemEventModel.setPickupPointCode(itemPickupPointVo.getPickupPointCode());
        itemEventModelList.add(itemEventModel);
      }
    }
    return itemEventModelList;
  }

  public String getCategoryCodes(String categoryCode) {
    if (StringUtils.isNotBlank(categoryCode)) {
      return categoryCode.split(Constants.DELIMETER)[1];
    }
    return null;
  }

  @Override
  public List<ItemSummaryListResponse> constructItemSummaryListResponse(List<ProductAndItemSolr> resultFromSolr, Map<String, ItemPickupPoint> itemPickupPointMap,
      Map<String, MasterDataItem> masterDataItems) {
    List<ItemSummaryListResponse> responseList = new ArrayList<>();
    for (ProductAndItemSolr item : resultFromSolr) {
      ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(item.getItemSku());
      ItemSummaryListResponse itemSummaryListResponse =
          ItemSummaryListResponse.builder().itemCode(item.getItemCode()).itemSku(item.getItemSku())
              .merchantSku(item.getMerchantSku()).productName(item.getProductName()).itemName(item.getItemName())
              .productSku(item.getProductSku()).merchantCode(item.getMerchantCode()).salesCategoryCode(
                  item.getSalesCatalog().stream().map(categoryCode -> getCategoryCodes(categoryCode)).collect(Collectors.toList())).masterCategoryCode(getCategoryCodes(item.getMasterCatalog())).build();
      if (Objects.nonNull(masterDataItems.get(item.getItemCode()))) {
        itemSummaryListResponse.setMainImageUrl(getMainImageUrl(masterDataItems.get(item.getItemCode())));
        itemSummaryListResponse.setDefiningAttributes(getDefiningAtrributeDetails(masterDataItems.get(item.getItemCode())));
      }
      setLateFulfillmentByProductType(item.getProductType(), itemSummaryListResponse);
      responseList.add(updateItemSummaryListResponse(itemPickupPoint, itemSummaryListResponse));
    }
    return responseList;
  }

  private void setLateFulfillmentByProductType(String productTypeValue,
      ItemSummaryListResponse itemSummaryListResponse) {
    ProductType productType = CommonUtil.getProductType(productTypeValue);
      itemSummaryListResponse.setLateFulfillment(
          CommonUtil.getLateFulfillmentFromProductType(productType, overrideLateFulfillmentByProductType,
              itemSummaryListResponse.isLateFulfillment()));
  }

  @Override
  public List<ItemSummaryListResponse> constructItemSummaryListResponseUsingNewMasterData(List<Item> items,
      Map<String, Product> productAndProductSkuMap, Map<String, ItemPickupPoint> itemPickupPointMap) {
    List<ItemSummaryListResponse> responseList = new ArrayList<>();
    for (Item item : items) {
      Product product = productAndProductSkuMap.get(item.getProductSku());
      ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(item.getItemSku());
      ItemSummaryListResponse itemSummaryListResponse =
          ItemSummaryListResponse.builder().itemCode(item.getItemCode()).itemSku(item.getItemSku())
              .merchantSku(item.getMerchantSku()).productName(product.getProductName()).productSku(item.getProductSku())
              .merchantCode(item.getMerchantCode()).itemName(item.getGeneratedItemName())
              .salesCategoryCode(getSalesCategoryCodesFromSalesCatalogs(product.getSalesCatalogs())).masterCategoryCode(item.getCategoryCode()).build();
      itemSummaryListResponse.setMainImageUrl(item.getMainImageUrl());
      itemSummaryListResponse.setDefiningAttributes(ResponseHelper.getDefiningAttributesFromProduct(product, item.getItemSku()).stream().map(
          productAttributeDetail -> new ProductAttributeDetailDTO(productAttributeDetail.getAttributeCode(),
              productAttributeDetail.getAttributeName(), productAttributeDetail.getAttributeValue(), null, null, null))
          .collect(Collectors.toList()));
      itemSummaryListResponse.setLateFulfillment(
          CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
              itemSummaryListResponse.isLateFulfillment()));
      responseList.add(updateItemSummaryListResponse(itemPickupPoint, itemSummaryListResponse));
    }
    return responseList;
  }

  @Override
  public List<String> getSalesCategoryCodesFromSalesCatalogs(List<SalesCatalog> salesCatalogs) {
    List<String> salesCategoryList = new ArrayList<>();
    for (SalesCatalog salesCatalog : Optional.ofNullable(salesCatalogs).orElse(new ArrayList<>())) {
      for (Category category : Optional.ofNullable(salesCatalog.getListOfCategories()).orElse(new ArrayList<>())) {
        salesCategoryList.add(category.getCategoryCode());
      }
    }
    return salesCategoryList;
  }

  private ItemSummaryListResponse updateItemSummaryListResponse(ItemPickupPoint itemPickupPoint,
      ItemSummaryListResponse itemSummaryListResponse) {
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
      itemSummaryListResponse.setItemViewConfigs(
          itemPickupPoint.getItemViewConfig().stream().map(this::convertToProductViewConfigResponse).collect(Collectors.toSet()));
    }
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
      itemSummaryListResponse.setPrice(
          itemPickupPoint.getPrice().stream().map(this::convertToItemPriceResponse).collect(Collectors.toSet()));
      itemSummaryListResponse.setOriginalSellingPrice(
          itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
    }
    return itemSummaryListResponse;
  }

  private List<ProductAttributeDetailDTO> getDefiningAtrributeDetails(MasterDataItem masterDataItem) {
    return masterDataItem.getMasterDataItemAttributeValues().stream().filter(
        attributeDetails -> MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
            attributeDetails.getMasterDataAttribute().getAttributeType())).map(
        attributeDetails -> new ProductAttributeDetailDTO(attributeDetails.getMasterDataAttribute().getAttributeCode(),
            attributeDetails.getMasterDataAttribute().getAttributeName(), attributeDetails.getAttributeValue(), null,
            null, null)).collect(Collectors.toList());
  }

  private String getMainImageUrl(MasterDataItem masterDataItem) {
    return masterDataItem.getMasterDataItemImages().stream().filter(MasterDataItemImage::isMainImage)
        .map(MasterDataItemImage::getLocationPath).findFirst().orElse(null);
  }

  private ItemViewConfigDTO convertToProductViewConfigResponse(ItemViewConfig itemViewConfig) {
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setBuyable(itemViewConfig.isBuyable());
    itemViewConfigDTO.setChannel(itemViewConfig.getChannel());
    itemViewConfigDTO.setDiscoverable(itemViewConfig.isDiscoverable());
    if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
      ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
      itemBuyableScheduleDTO.setBuyable(itemViewConfig.getItemBuyableSchedules().isBuyable());
      itemBuyableScheduleDTO.setStartDateTime(itemViewConfig.getItemBuyableSchedules().getStartDateTime());
      itemBuyableScheduleDTO.setEndDateTime(itemViewConfig.getItemBuyableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
    }
    if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
      ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
      itemDiscoverableScheduleDTO.setDiscoverable(itemViewConfig.getItemDiscoverableSchedules().isDiscoverable());
      itemDiscoverableScheduleDTO.setStartDateTime(itemViewConfig.getItemDiscoverableSchedules().getStartDateTime());
      itemDiscoverableScheduleDTO.setEndDateTime(itemViewConfig.getItemDiscoverableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    }
    return itemViewConfigDTO;
  }

  private PriceDTO convertToItemPriceResponse(Price price) {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setCurrency(price.getCurrency());
    priceDTO.setChannel(price.getChannel());
    priceDTO.setOfferPrice(price.getOfferPrice());
    priceDTO.setListPrice(price.getListPrice());
    priceDTO.setLastUpdatedBy(price.getLastUpdatedBy());
    priceDTO.setLastUpdatedDate(price.getLastUpdatedDate());
    if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
      DiscountPrice discountPriceFinal =
        itemHelperService.processDiscountPricesByPriority(price.getListOfDiscountPrices());
      if (Objects.nonNull(discountPriceFinal)) {
        priceDTO.setListOfDiscountPrices(new ArrayList<>(
          Collections.singletonList(convertToDiscountPriceDTO(discountPriceFinal))));
      }
      else{
        priceDTO.setListOfDiscountPrices(Collections.emptyList());
      }
    }
    if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
      priceDTO.setMerchantPromoDiscountPrice(convertToDiscountPriceDTO(price.getMerchantPromoDiscountPrice()));
    }
    return priceDTO;
  }

  private DiscountPriceDTO convertToDiscountPriceDTO(DiscountPrice discountPrice) {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    discountPriceDTO.setDiscountPrice(discountPrice.getDiscountPrice());
    discountPriceDTO.setStartDateTime(discountPrice.getStartDateTime());
    discountPriceDTO.setEndDateTime(discountPrice.getEndDateTime());
    discountPriceDTO.setAdjustmentName(discountPrice.getAdjustmentName());
    if (Objects.nonNull(discountPrice.getAdjustmentType())) {
      discountPriceDTO.setAdjustmentType(AdjustmentTypeEnum.valueOf(discountPrice.getAdjustmentType().toString()));
    }
    return discountPriceDTO;
  }

  @Override
  public void overrideL4DetailsFromL5(List<Item> items, List<ItemPickupPoint> itemPickupPoints) {
    Map<String, ItemPickupPoint> itemSkuAndItemPickupPointMap =
        Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
            .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (a, b) -> b));
    // Do not override L4 version with L5 as it would lead to optimistic locking exception in multiple places
    for (Item item : items) {
        if (Objects.nonNull(item) && itemSkuAndItemPickupPointMap.containsKey(item.getItemSku())) {
          ItemPickupPoint itemPickupPoint = itemSkuAndItemPickupPointMap.get(item.getItemSku());
          item.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
          item.setPrice(itemPickupPoint.getPrice());
          item.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
          item.setPromoBundling(itemPickupPoint.isPromoBundling());
          item.setPickupPointCode(itemPickupPoint.getPickupPointCode());
          item.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
          item.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
          item.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
          item.setPickupPointCode(itemPickupPoint.getPickupPointCode());
          item.setMerchantSku(itemPickupPoint.getMerchantSku());
        }
    }
  }

  @Override
  public void overrideL5DetailsFromL4(List<Item> items, List<ItemPickupPoint> itemPickupPoints) {
    Map<String, ItemPickupPoint> itemSkuAndItemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    for (Item item : items) {
      if (itemSkuAndItemPickupPointMap.containsKey(item.getItemSku())) {
        ItemPickupPoint itemPickupPoint = itemSkuAndItemPickupPointMap.get(item.getItemSku());
        itemPickupPoint.setItemViewConfig(item.getItemViewConfigs());
        CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, channelService.getDefaultChannel());
        itemPickupPoint.setPrice(item.getPrice());
        itemPickupPoint.setWholesalePriceExists(item.isWholesalePriceExists());
        itemPickupPoint.setPromoBundling(item.isPromoBundling());
        itemPickupPoint.setPickupPointCode(item.getPickupPointCode());
        itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
        itemPickupPoint.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
        itemPickupPoint.setFlashSaleActive(item.isFlashSaleActive());
      }
    }
  }

  @Override
  public ItemPickupPointDataChangeEventModel convertToItemPickupPointChangeEventModel(ItemPickupPoint itemPickupPoint,
      Boolean pureCNCStatusChange) {
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setStoreId(itemPickupPoint.getStoreId());
    itemPickupPointDataChangeEventModel.setMerchantCode(itemPickupPoint.getMerchantCode());
    itemPickupPointDataChangeEventModel.setItemSku(itemPickupPoint.getItemSku());
    itemPickupPointDataChangeEventModel.setProductSku(itemPickupPoint.getProductSku());
    itemPickupPointDataChangeEventModel.setMerchantSku(itemPickupPoint.getMerchantSku());
    itemPickupPointDataChangeEventModel.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemPickupPointDataChangeEventModel.setExternalPickupPointCode(itemPickupPoint.getExternalPickupPointCode());
    itemPickupPointDataChangeEventModel.setCncActive(itemPickupPoint.isCncActive());
    itemPickupPointDataChangeEventModel.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    itemPickupPointDataChangeEventModel.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    itemPickupPointDataChangeEventModel.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
    itemPickupPointDataChangeEventModel.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemPickupPointDataChangeEventModel.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    itemPickupPointDataChangeEventModel.setFbbActivated(itemPickupPoint.isFbbActivated());
    itemPickupPointDataChangeEventModel.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    itemPickupPointDataChangeEventModel.setNewData(itemPickupPoint.getNewData());
    itemPickupPointDataChangeEventModel.setPrice(getEventModelPrice(itemPickupPoint));
    itemPickupPointDataChangeEventModel.setPureCNCStatusChange(Boolean.TRUE.equals(pureCNCStatusChange));
    itemPickupPointDataChangeEventModel.setItemViewConfigs(getEventModelItemViewConfig(itemPickupPoint));
    itemPickupPointDataChangeEventModel.setForceReview(itemPickupPoint.isForceReview());
    itemPickupPointDataChangeEventModel.setDistribution(itemPickupPoint.isDistribution());
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemPickupPointDataChangeType())) {
      for (String val : itemPickupPoint.getItemPickupPointDataChangeType()) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.findEnumByValue(val));
      }
    }
    itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
    itemPickupPointDataChangeEventModel.setB2bFields(
        Objects.nonNull(itemPickupPoint.getB2bFields()) ? itemPickupPoint.getB2bFields() : new B2bFields());
    itemPickupPointDataChangeEventModel
        .setOfflineItemId(itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode());
    return itemPickupPointDataChangeEventModel;
  }

  private Set<com.gdn.x.product.domain.event.model.ItemViewConfig> getEventModelItemViewConfig(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getAllItemViewConfigs().stream()
        .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, com.gdn.x.product.domain.event.model.ItemViewConfig.class))
        .collect(Collectors.toSet());
  }

  private Set<com.gdn.x.product.domain.event.model.Price> getEventModelPrice(ItemPickupPoint itemPickupPoint) {
    Set<com.gdn.x.product.domain.event.model.Price> prices = new HashSet<>();
    try {
      for (Price itemPrice : itemPickupPoint.getPrice()) {
        com.gdn.x.product.domain.event.model.Price price = new com.gdn.x.product.domain.event.model.Price();
        org.springframework.beans.BeanUtils.copyProperties(itemPrice, price);
        prices.add(price);
      }
    } catch (Exception ex) {
      LOGGER.error("Error while copying Price :{}", itemPickupPoint.getPrice(), ex);
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "Error while copying Item Price");
    }
    return prices;
  }

  @Override
  public BusinessPartnerPickupPoint convertToBusinessPartnerPickupPoint(PickupPointVOEventModel pickupPointVO,
      String businessPartnerCode) {
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode(businessPartnerCode);
    BeanUtils.copyProperties(pickupPointVO, businessPartnerPickupPoint, "businessHours", "businessPartnerCode");
    if (Objects.nonNull(pickupPointVO.getContactPerson())) {
      businessPartnerPickupPoint.setContactPersonName(pickupPointVO.getContactPerson().getName());
      businessPartnerPickupPoint.setContactEmail(pickupPointVO.getContactPerson().getEmail());
      businessPartnerPickupPoint.setContactTelephone(pickupPointVO.getContactPerson().getPhone());
      businessPartnerPickupPoint.setContactTelephoneExt(pickupPointVO.getContactPerson().getPhoneExt());
    }
    if (Objects.nonNull(pickupPointVO.getGeolocation())) {
      Geolocation geolocation = new Geolocation();
      geolocation.setLongitude(pickupPointVO.getGeolocation().getLongitude());
      geolocation.setLatitude(pickupPointVO.getGeolocation().getLatitude());
      geolocation.setPlaceId(pickupPointVO.getGeolocation().getPlaceId());
      geolocation.setStreetAddress(pickupPointVO.getGeolocation().getStreetAddress());
      businessPartnerPickupPoint.setGeolocation(geolocation);
    }
    if (CollectionUtils.isNotEmpty(pickupPointVO.getBusinessHours())) {
      List<BusinessHour> businessHourList = new ArrayList<>();
      for (BusinessHourVOEventModel businessHourVO : pickupPointVO.getBusinessHours()) {
        BusinessHour businessHour = new BusinessHour();
        BeanUtils.copyProperties(businessHourVO, businessHour, "DayOfWeek");
        businessHour.setDay(DayOfWeek.valueOf(businessHourVO.getDay().name()));
        businessHourList.add(businessHour);
      }
      businessPartnerPickupPoint.setBusinessHours(businessHourList);
    }
    if (Objects.nonNull(pickupPointVO.getFlags())) {
      if (Objects.nonNull(pickupPointVO.getFlags().getDeliveryFlag())) {
        businessPartnerPickupPoint.setDelivery(pickupPointVO.getFlags().getDeliveryFlag());
      }
      businessPartnerPickupPoint.setDistribution(
          Boolean.TRUE.equals(pickupPointVO.getFlags().getWarehouseDistribution()));
    }
    if (Objects.nonNull(pickupPointVO.getFlags()) &&
        Objects.nonNull(pickupPointVO.getFlags().getDeliveryFlag())) {
      businessPartnerPickupPoint.setDelivery(Boolean.TRUE.equals(pickupPointVO.getFlags().getDeliveryFlag()));
    }
    return businessPartnerPickupPoint;
  }

  @Override
  public BusinessPartnerPickupPoint convertToBusinessPartnerPickupPointFromPickupPointChange(
      PickupPointVOEventModel pickupPointChange, String businessPartnerCode,
      BusinessPartnerPickupPoint businessPartnerPickupPoint) {
    businessPartnerPickupPoint.setBusinessPartnerCode(businessPartnerCode);
    BeanUtils.copyProperties(pickupPointChange, businessPartnerPickupPoint, "businessHours", "id", "createdDate",
        "createdBy", "businessPartnerCode");
    if (Objects.nonNull(pickupPointChange.getContactPerson())) {
      businessPartnerPickupPoint.setContactPersonName(pickupPointChange.getContactPerson().getName());
      businessPartnerPickupPoint.setContactEmail(pickupPointChange.getContactPerson().getEmail());
      businessPartnerPickupPoint.setContactTelephone(pickupPointChange.getContactPerson().getPhone());
      businessPartnerPickupPoint.setContactTelephoneExt(pickupPointChange.getContactPerson().getPhoneExt());
    }
    if (Objects.nonNull(pickupPointChange.getGeolocation())) {
      Geolocation geolocation = new Geolocation();
      geolocation.setLongitude(pickupPointChange.getGeolocation().getLongitude());
      geolocation.setLatitude(pickupPointChange.getGeolocation().getLatitude());
      geolocation.setPlaceId(pickupPointChange.getGeolocation().getPlaceId());
      geolocation.setStreetAddress(pickupPointChange.getGeolocation().getStreetAddress());
      businessPartnerPickupPoint.setGeolocation(geolocation);
    }
    if (CollectionUtils.isNotEmpty(pickupPointChange.getBusinessHours())) {
      List<BusinessHour> businessHourList = new ArrayList<>();
      for (BusinessHourVOEventModel businessHourVO : pickupPointChange.getBusinessHours()) {
        if (businessHourVO.getDay() == null) {
          continue;
        }
        BusinessHour businessHour = new BusinessHour();
        BeanUtils.copyProperties(businessHourVO, businessHour, "DayOfWeek");
        businessHour.setDay(DayOfWeek.valueOf(businessHourVO.getDay().name()));
        businessHourList.add(businessHour);
      }
      businessPartnerPickupPoint.setBusinessHours(businessHourList);
    }
    if (Objects.nonNull(pickupPointChange.getFlags())) {
      businessPartnerPickupPoint.setDelivery(Boolean.TRUE.equals(pickupPointChange.getFlags().getDeliveryFlag()));
      businessPartnerPickupPoint.setDistribution(
          Boolean.TRUE.equals(pickupPointChange.getFlags().getWarehouseDistribution()));
    }

    return businessPartnerPickupPoint;
  }

  @Override
  public ProductAttributeDetail toProductAttributeDetail(
      ProductAttributeDomainEventModel productAttributeDomainEventModel) {
    ProductAttributeDetail productAttributeDetail = ProductAttributeDetail.builder()
        .attributeCode(productAttributeDomainEventModel.getAttribute().getAttributeCode())
        .attributeName(productAttributeDomainEventModel.getAttribute().getName()).build();
    if (MasterDataAttributeType.DEFINING_ATTRIBUTE.name().equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
      productAttributeDetail.setAttributeValue(
          productAttributeDomainEventModel.getProductAttributeValues().get(0).getAllowedAttributeValue().getValue());
    } else {
      productAttributeDetail.setAttributeValue(
          productAttributeDomainEventModel.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    }
    return productAttributeDetail;
  }


  @Override
  public ProductAttributeDetail toProductAttributeDetail(ProductItemAttributeValueResponse productItemAttributeValueResponse) {
    ProductAttributeDetail productAttributeDetail = ProductAttributeDetail.builder()
        .attributeCode(productItemAttributeValueResponse.getAttributeResponse().getAttributeCode())
        .attributeName(productItemAttributeValueResponse.getAttributeResponse().getName()).build();
    productAttributeDetail.setAttributeValue(productItemAttributeValueResponse.getValue());
    return productAttributeDetail;
  }

  @Override
  public List<ItemLevel4ListingResponse> convertItemToItemLevel4SummaryResponse(List<Item> itemSet, String requestId,
      List<ItemPickupPoint> itemPickupPointByProductSkus, Map<String, Boolean> productSkuAndSharedProductMap)
      throws Exception {
    List<ItemLevel4ListingResponse> itemLevel4ListingResponses = new ArrayList<>();
    Map<String, ItemImageResponse> itemCodeItemResponseMap = new HashMap<>();
    Map<String, List<String>> itemSkuAndPickupPointCodes =
        Optional.ofNullable(itemPickupPointByProductSkus).orElseGet(Collections::emptyList).stream().collect(Collectors
            .groupingBy(ItemPickupPoint::getItemSku, Collectors.mapping(ItemPickupPoint::getPickupPointCode,
                Collectors.collectingAndThen(Collectors.toSet(), ArrayList::new))));
    Map<String, List<String>> itemSkuAndFbbPickupPointCodes =
      Optional.ofNullable(itemPickupPointByProductSkus).orElseGet(Collections::emptyList).stream()
        .filter(ItemPickupPoint::isFbbActivated).collect(Collectors
        .groupingBy(ItemPickupPoint::getItemSku, Collectors
          .mapping(ItemPickupPoint::getPickupPointCode,
            Collectors.collectingAndThen(Collectors.toSet(), ArrayList::new))));
    if (CollectionUtils.isNotEmpty(itemSet) && Boolean.TRUE.equals(itemSet.get(0).isSynchronized())) {
      GdnRestListResponse<ItemImageResponse> productItemDetailsFromPCB =
        productCategoryBaseClient.getProductItemImagesByItemCode(
          fetchSkuCodeRequestForItemDetails(itemSet));
     itemCodeItemResponseMap =
        Objects.isNull(productItemDetailsFromPCB.getContent()) ?
          Collections.emptyMap() :
          productItemDetailsFromPCB.getContent().stream()
            .collect(Collectors.toMap(ItemImageResponse::getItemCode, Function.identity()));

    }
    if (CollectionUtils.isNotEmpty(itemSet)) {
      for (Item item : itemSet) {
        ItemLevel4ListingResponse response =
          gdnMapper.deepCopy(item, ItemLevel4ListingResponse.class);
        response.setSellerSku(Optional.of(itemSet.get(0)).map(Item::getMerchantSku).orElse(null));
        if (!item.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
          response.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
          response.setUpcCode(item.getMasterDataItem().getUpcCode());
          response.setIsSynchronized(Boolean.FALSE);
        } else {
          if (itemCodeItemResponseMap.containsKey(item.getItemCode())) {
            ItemImageResponse itemImageResponse = itemCodeItemResponseMap.get(item.getItemCode());
            response.setGeneratedItemName(itemImageResponse.getItemName());
            response.setUpcCode(itemImageResponse.getUpcCode());
            response.setIsSynchronized(Boolean.TRUE);
          }
        }
        if (CollectionUtils.isNotEmpty(item.getBundleRecipe())) {
          response.setBundleItemResponses(
              getBundleInfo(item.getStoreId(), item.getBundleRecipe(), productSkuAndSharedProductMap));
        }
        response.setPickupPointCodes(itemSkuAndPickupPointCodes.get(response.getItemSku()));
        response.setFbbPickupPointCodes(
          Objects.nonNull(itemSkuAndFbbPickupPointCodes.get(response.getItemSku())) ?
            itemSkuAndFbbPickupPointCodes.get(response.getItemSku()) :
            new ArrayList<>());
        if(CollectionUtils.isNotEmpty(response.getPickupPointCodes())) {
          response.getPickupPointCodes().removeAll(response.getFbbPickupPointCodes());
        }
        response.setSharedProduct(productSkuAndSharedProductMap.getOrDefault(item.getProductSku(), false));
        itemLevel4ListingResponses.add(response);
      }
    }
    return itemLevel4ListingResponses;
  }

  private static SkuCodesRequest fetchSkuCodeRequestForItemDetails(List<Item> itemSet) {
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchImageResponse(false);
    skuCodesRequest.setSkuCodes(itemSet.stream().map(Item::getItemCode).collect(toList()));
    return skuCodesRequest;
  }

  private List<BundleItemResponse> getBundleInfo(String storeId, Set<BundleRecipe> bundleRecipes,
      Map<String, Boolean> productSkuAndSharedProductMap) {
    List<BundleItemResponse> bundleItemResponses = new ArrayList<>();
    Set<String> itemSkus = bundleRecipes.stream().map(BundleRecipe::getItemSku).collect(Collectors.toSet());
    List<Item> items = itemService.getItemsByStoreIdAndItemSkus(storeId, itemSkus);
    Map<String, Item> itemSkuAndItemMap =
        items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    for (BundleRecipe bundleRecipe : bundleRecipes) {
      bundleItemResponses.add(getBundleRecipe(itemSkuAndItemMap.get(bundleRecipe.getItemSku()), bundleRecipe,
          productSkuAndSharedProductMap));
    }
    return bundleItemResponses;
  }

  private BundleItemResponse getBundleRecipe(Item item, BundleRecipe bundleRecipe,
      Map<String, Boolean> productSkuAndSharedProductMap) {
    BundleItemResponse bundleItemResponse = new BundleItemResponse();
    if (Objects.nonNull(item)) {
      bundleItemResponse.setMainImageUrl(item.getMainImageUrl());
      bundleItemResponse.setItemCode(item.getItemCode());
      bundleItemResponse.setItemName(item.getGeneratedItemName());
      bundleItemResponse.setProductSku(item.getProductSku());
      bundleItemResponse.setProductStatus(ResponseHelper.getProductStatus(item));
      bundleItemResponse.setSharedProduct(productSkuAndSharedProductMap.getOrDefault(item.getProductSku(), false));
    }
    bundleItemResponse.setItemSku(bundleRecipe.getItemSku());
    bundleItemResponse.setQuantity(bundleRecipe.getQuantity());
    return bundleItemResponse;
  }

  @Override
  public List<ItemLevel5Response> convertItemToItemLevel5Response(String storeId, List<ItemPickupPoint> itemPickupPointList, List<Item> items,
      List<Product> productList, boolean fetchB2bData,
      Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache, boolean cncForWarehouseFeatureSwitch,
      String fetchViewConfigByChannel) throws Exception {
    List<ItemLevel5Response> itemLevel5Responses = new ArrayList<>();
    Map<String, Product> productMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productList)) {
      productMap = productList.stream().filter(Objects::nonNull)
          .collect(Collectors.toMap(Product::getProductSku, Function.identity(), (oldValue, newValue) -> newValue));
    }
    for (Item item : items) {
      if (!item.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
        item.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
      }
    }
    Map<String, Item> itemSkuItemMap =
        items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldValue, newValue) -> newValue));
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemSkuItemMap.get(itemPickupPoint.getItemSku());
      ItemLevel5Response itemLevel5Response = gdnMapper.deepCopy(item, ItemLevel5Response.class);
      BeanUtils.copyProperties(itemPickupPoint, itemLevel5Response, "prices", "viewConfigs");
      List<PriceResponse> prices = new ArrayList<>();
      List<ViewConfigResponse> viewConfigs = new ArrayList<>();
      for (Price price : itemPickupPoint.getPrice()) {
        PriceResponse priceResponse = new PriceResponse();
        priceResponse.setChannelId(price.getChannel());
        priceResponse.setPrice(price.getListPrice());
        priceResponse.setSalePrice(price.getOfferPrice());
        if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
          priceResponse.setDiscountAmount(price.getListOfDiscountPrices().get(0).getDiscountPrice());
          priceResponse.setDiscountStartDate(price.getListOfDiscountPrices().get(0).getStartDateTime());
          priceResponse.setDiscountStartDate(price.getListOfDiscountPrices().get(0).getEndDateTime());
        }
        prices.add(priceResponse);
        itemLevel5Response.setOriginalSellingPrice(price.getOfferPrice());
      }
      Pair<Set<String>, Boolean> allowedConfigSet =
          ResponseHelper.getRequestViewConfigSet(fetchViewConfigByChannel, cncForWarehouseFeatureSwitch);
      for(ItemViewConfig itemViewConfig : itemPickupPoint.getAllItemViewConfigs()){
        if (allowedConfigSet.getSecond() && !allowedConfigSet.getFirst()
            .contains(itemViewConfig.getChannel())) {
          continue;
        }
        ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
        viewConfigResponse.setBuyable(itemViewConfig.isBuyable());
        viewConfigResponse.setDisplay(itemViewConfig.isDiscoverable());
        viewConfigResponse.setChannelId(itemViewConfig.getChannel());
        ResponseHelper.setBuyableScheduleRequest(viewConfigResponse, itemViewConfig.getItemBuyableSchedules());
        ResponseHelper.setDiscoverableScheduleRequest(viewConfigResponse, itemViewConfig.getItemDiscoverableSchedules());
        viewConfigs.add(viewConfigResponse);
      }
      if (fetchB2bData) {
        B2BResponse b2BResponse = new B2BResponse();
        if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
          BeanUtils.copyProperties(itemPickupPoint.getB2bFields(), b2BResponse);
        }
        itemLevel5Response.setB2BResponse(b2BResponse);
      }
      itemLevel5Response.setPrices(prices);
      itemLevel5Response.setViewConfigs(viewConfigs);

      if (MapUtils.isNotEmpty(productMap) && productMap.containsKey(itemPickupPoint.getProductSku())) {
        Product product = productMap.get(itemPickupPoint.getProductSku());
        itemLevel5Response.setProductName(product.getProductName());
        itemLevel5Response.setProductType(product.getProductType());
        itemLevel5Response.setCategoryCode(product.getCategoryCode());
        itemLevel5Response.setProductCode(product.getProductCode());
        itemLevel5Response.setPreOrder(ResponseHelper.getPreOrder(product.getPreOrder()));
      } else {
        LOGGER.info(
          "product Details missing from itemLevel5Response for Product not found for L5 : {} ",
          itemPickupPoint.getOfflineItemId());
      }
      itemLevel5Response.setSellerSku(itemPickupPoint.getMerchantSku());
      itemLevel5Response.setCncActive(
          ResponseHelper.setCncActivatedForBackward(itemPickupPoint.getItemViewConfigByChannel(Constants.CNC),
              cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(), ItemViewConfig::getChannel,
              ItemViewConfig::isBuyable));
      itemLevel5Response.setIsSynchronized(item.isSynchronized());
      itemLevel5Response.setIsArchived(item.isArchived());
      itemLevel5Response.setGeneratedItemName(item.getGeneratedItemName());
      itemLevel5Response.setBundleItemResponses(toBundleItemResponseList(item));
      itemLevel5Response.setDistribution(itemPickupPoint.isDistribution());
      itemLevel5Response.setCategoryHierarchy(
          ResponseHelper.toCategoryDataResponse(parentCategoriesFromDbAndCache, itemLevel5Response.getCategoryCode()));
      itemLevel5Responses.add(itemLevel5Response);
    }
    return itemLevel5Responses;
  }

  private List<BundleItemResponse> toBundleItemResponseList(Item item) {
    return Optional.ofNullable(item).map(Item::getBundleRecipe).orElse(new HashSet<>()).stream()
        .map(this::toBundleItemResponse).collect(toList());
  }

  private BundleItemResponse toBundleItemResponse(BundleRecipe bundleRecipe) {
    BundleItemResponse bundleItemResponse = new BundleItemResponse();
    BeanUtils.copyProperties(bundleRecipe, bundleItemResponse);
    return bundleItemResponse;
  }

  @Override
  public void overrideDefiningAttributeDetailsFromL3ToL4(List<Product> products, List<Item> items) {
    Map<String, List<ProductAttributeDetail>> itemAttributeDetailsMap =
        products.stream().map(product -> Optional.ofNullable(product.getDefiningAttributes()).orElse(new ArrayList<>()))
            .flatMap(List::stream)
            .filter(productAttribute -> CollectionUtils.isNotEmpty(productAttribute.getProductAttributeDetails()))
            .collect(Collectors.toMap(ProductAttribute::getItemSku, ProductAttribute::getProductAttributeDetails));
    items.forEach(item -> item.setDefiningAttributes(
        Optional.ofNullable(itemAttributeDetailsMap.get(item.getItemSku())).orElse(new ArrayList<>())));
  }


  @Override
  public ItemBasicDetailResponse convertItemPickupPointsAndProductToItemBasicDetailResponse(
      Page<ItemPickupPoint> itemPickupPoints, Product product, String itemSku) {

    ItemBasicDetailResponse itemBasicDetailResponse = new ItemBasicDetailResponse();
    itemBasicDetailResponse.setMerchantCode(product.getMerchantCode());
    itemBasicDetailResponse.setItemSku(itemSku);
    List<ItemPickupPointResponse> itemPickupPointResponseList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints.getContent()) {
      ItemPickupPointResponse itemPickupPointResponse = new ItemPickupPointResponse();
      itemPickupPointResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      itemPickupPointResponse.setBuyable(CommonUtil.getBuyableFromConfig(itemPickupPoint
          .getItemViewConfig().stream().findFirst().get()));
      itemPickupPointResponse.setDiscoverable(CommonUtil.getDiscoverableFromConfig(itemPickupPoint
          .getItemViewConfig().stream().findFirst().get()));
      itemPickupPointResponse.setCncActive(
          ResponseHelper.setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
              cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(), ItemViewConfig::getChannel,
              ItemViewConfig::isBuyable));
      itemPickupPointResponseList.add(itemPickupPointResponse);
    }
    itemBasicDetailResponse.setItemPickupPoints(new PageImpl<>(itemPickupPointResponseList,
        PageRequest.of(itemPickupPoints.getNumber(), itemPickupPoints.getSize()),
        itemPickupPoints.getTotalElements()));
    itemBasicDetailResponse.setProductTypeCode(product.getProductType().getCode());
    itemBasicDetailResponse.setProductTypeName(product.getProductType().getDescription());
    itemBasicDetailResponse.setOnline(product.isOnline());
    return itemBasicDetailResponse;
  }

  @Override
  public ActivateNeedRevisionResponse convertActivateNeedRevisionResponseList(boolean createdNew, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints) {
    ActivateNeedRevisionResponse activateNeedRevisionResponse = new ActivateNeedRevisionResponse();
    activateNeedRevisionResponse.setCreateNew(createdNew);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Map<String, Item> itemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
      List<NewlyAddedL5Response> newlyAddedL5Responses = new ArrayList<>();
      for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
        NewlyAddedL5Response newlyAddedL5Response = new NewlyAddedL5Response();
        newlyAddedL5Response.setItemSku(itemPickupPoint.getItemSku());
        newlyAddedL5Response.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        newlyAddedL5Response.setItemName(itemMap.get(itemPickupPoint.getItemSku()).getGeneratedItemName());
        newlyAddedL5Response.setItemCode(itemMap.get(itemPickupPoint.getItemSku()).getItemCode());
        newlyAddedL5Responses.add(newlyAddedL5Response);
      }
      activateNeedRevisionResponse.setNewlyAddedL5Responses(newlyAddedL5Responses);
    }
    return activateNeedRevisionResponse;
  }

  @Override
  public List<AdjustmentProductChange> convertToAdjustmentProductChangeList(
    List<AdjustmentProductChangeResponseVO> adjustmentProductBySkuAndPickupPointCode,
    List<AdjustmentProductResponse> adjustmentProduct) {
    List<AdjustmentProductChange> adjustmentProductChangeList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(adjustmentProductBySkuAndPickupPointCode)) {
      for (AdjustmentProductChangeResponseVO response : adjustmentProductBySkuAndPickupPointCode) {
        AdjustmentProductChange adjustmentProductChange =
          gdnMapper.deepCopy(response, AdjustmentProductChange.class);
        adjustmentProductChangeList.add(adjustmentProductChange);
      }
    } else {
      for (AdjustmentProductResponse adjustmentProductResponse : adjustmentProduct) {
        AdjustmentProductChange adjustmentProductChange =
          gdnMapper.deepCopy(adjustmentProductResponse, AdjustmentProductChange.class);
        adjustmentProductChangeList.add(adjustmentProductChange);
      }
    }
    return adjustmentProductChangeList;
  }

  @Override
  public DuplicateProductDetailsResponse convertProductToDuplicateProductDetailsResponse(Product product) {
    DuplicateProductDetailsResponse duplicateProductDetailsResponse = new DuplicateProductDetailsResponse();
    duplicateProductDetailsResponse.setProductSku(product.getProductSku());
    duplicateProductDetailsResponse.setProductName(product.getProductName());
    if (product.isSuspended()) {
      duplicateProductDetailsResponse.setStatus(Constants.SUSPENDED);
    } else if (product.isArchived()) {
      duplicateProductDetailsResponse.setStatus(Constants.ARCHIVED);
    } else {
      duplicateProductDetailsResponse.setStatus(Constants.ACTIVE);
    }
    return duplicateProductDetailsResponse;
  }

  public ProductAndItemsSummaryResponseV2 constructProductAndItemsForView(Product product,
    Map<String, Item> itemMap, Map<String, ItemPickupPoint> itemPickupPointMap) throws Exception {
    //Setting Product Response for View
    ProductDetailResponseV2 productDetailResponseV2 = setProductResponseForView(product, itemMap);
    List<ProductItemDetailResponse> productItemDetails = new ArrayList<>();
    Map<String, ProductItemDetailResponse> productItemResponseMap = new HashMap<>();
    if (MapUtils.isNotEmpty(itemMap) && Boolean.TRUE.equals(
      itemMap.values().stream().anyMatch(Item::isSynchronized))) {
      productItemDetails = productCategoryBaseClient.findProductItemDetailsBySkuCodes(
        itemMap.values().stream().map(Item::getItemCode).collect(toList()), false, false);
      productItemResponseMap = productItemDetails.stream()
        .collect(Collectors.toMap(ProductItemDetailResponse::getSkuCode, Function.identity()));
    }
    //Getting list of Item Responses for View
    List<ItemResponse> itemResponses =
      getItemResponsesForView(itemMap, itemPickupPointMap, productItemResponseMap, product);
    //call business partner promo to populate seller promo bundling
    setSellerPromoBundlingForView(product.getStoreId(), itemResponses);
    return new ProductAndItemsSummaryResponseV2(productDetailResponseV2, itemResponses);
  }

  private void setSellerPromoBundlingForView(String storeId, List<ItemResponse> itemResponses) {
    List<String> businessPartnerCodeList =
            itemResponses.stream().map(ItemResponse::getMerchantCode).distinct()
                    .collect(Collectors.toList());
    Map<String, BusinessPartnerPromo> businessPartnerAndCodeMap =
            getBusinessPartnerPromoMap(storeId, businessPartnerCodeList);
    for (ItemResponse itemResponse : itemResponses) {
      BusinessPartnerPromo businessPartnerPromo =
              businessPartnerAndCodeMap.getOrDefault(itemResponse.getMerchantCode(), null);
      if (Objects.nonNull(businessPartnerPromo)) {
        itemResponse.setSellerActivePromoBundlings(businessPartnerPromo.getActivePromoBundlings());
      }
    }
  }

  private Map<String, BusinessPartnerPromo> getBusinessPartnerPromoMap(String storeId, List<String> businessPartnerCodeList) {
    return Optional.ofNullable(
            businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(storeId,
                    businessPartnerCodeList)).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(BusinessPartnerPromo::getBusinessPartnerCode, Function.identity()));
  }

  private List<ItemResponse> getItemResponsesForView(Map<String, Item> itemMap, Map<String, ItemPickupPoint> itemPickupPointMap,
      Map<String, ProductItemDetailResponse> productItemResponseMap, Product product) {
    List<ItemResponse> itemResponses = new ArrayList<>();
    for (Map.Entry<String, Item> itemEntry : itemMap.entrySet()) {
      Item item = itemEntry.getValue();
      ItemResponse itemResponse = gdnMapper.deepCopy(item, ItemResponse.class);
      BeanUtils.copyProperties(item, itemResponse);
      itemResponse.setActivePromoBundlings(
        Optional.ofNullable(item.getActivePromoBundlings()).orElse(new HashSet<>()));
      itemResponse.setOff2OnChannelActive(item.isOff2OnChannelActive());
      itemResponse.setCncActivated(item.isCncActivated());
      //get MasterDataItem for item Response
      MasterDataItemDTO masterDataItemDTO =
        getMasterDataItemDTO(productItemResponseMap, item, itemResponse);
      itemResponse.setMasterDataItem(masterDataItemDTO);
      itemResponse.getMasterDataItem().setMasterDataItemAttributeValues(
          this.convertProductAttributeDetailToMasterDataItemAttributeDTO(
              ResponseHelper.getDefiningAttributesFromProduct(product, item.getItemSku())));
      if (itemPickupPointMap.containsKey(itemResponse.getItemSku())) {
        ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(itemResponse.getItemSku());
        if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
          itemResponse.setItemViewConfigs(itemPickupPoint.getItemViewConfig().stream()
            .map(this::convertToProductViewConfigResponse).collect(Collectors.toSet()));
        }
        setSubscribableBasedOnPreferredSubscriptionType(item, itemResponse, itemPickupPoint);
        itemResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        itemResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
        itemResponse.setCncActive(itemPickupPoint.isCncActive());
        if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
          itemResponse.setPrice(
            itemPickupPoint.getPrice().stream().map(this::convertToItemPriceResponse)
              .collect(Collectors.toSet()));
        }
      }
      if(Objects.nonNull(item.getPristineDataItem())){
        PristineDataItemDto pristineDataItem = new PristineDataItemDto();
        pristineDataItem.setPristineId(item.getPristineDataItem().getPristineId());
        itemResponse.setPristineDataItem(pristineDataItem);
      }
      itemResponses.add(itemResponse);
    }
    return itemResponses;
  }

  private void setSubscribableBasedOnPreferredSubscriptionType(Item item, ItemResponse itemResponse,
      ItemPickupPoint itemPickupPoint) {
    if (item.isSubscribable()) {
      if (CollectionUtils.isEmpty(item.getPreferredSubscriptionType()) || Stream.of(WAREHOUSE, MARKETPLACE)
          .allMatch(item.getPreferredSubscriptionType()::contains)) {
        itemResponse.setSubscribable(true);
      } else if (item.getPreferredSubscriptionType().size() == 1 && item.getPreferredSubscriptionType()
          .contains(WAREHOUSE)) {
        itemResponse.setSubscribable(itemPickupPoint.isFbbActivated());
      } else if (item.getPreferredSubscriptionType().size() == 1 && item.getPreferredSubscriptionType()
          .contains(MARKETPLACE)) {
        itemResponse.setSubscribable(!itemPickupPoint.isFbbActivated());
      }
    } else {
      itemResponse.setSubscribable(false);
    }
  }

  private MasterDataItemDTO getMasterDataItemDTO(
    Map<String, ProductItemDetailResponse> productItemResponseMap, Item item,
    ItemResponse itemResponse) {
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    if (!item.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
      masterDataItemDTO.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
      masterDataItemDTO.setSkuCode(item.getMasterDataItem().getSkuCode());
      masterDataItemDTO.setUpcCode(item.getMasterDataItem().getUpcCode());
      masterDataItemDTO.setDangerousLevel(item.getDangerousLevel());
      masterDataItemDTO.setMasterDataItemImages(
        Optional.ofNullable(item.getMasterDataItem().getMasterDataItemImages())
          .orElse(new ArrayList<>()).stream()
          .map(masterDataItemImage -> convertToMasterDataItemImageDTO(masterDataItemImage, null))
          .collect(toList()));
    } else {
      if (productItemResponseMap.containsKey(itemResponse.getItemCode())) {
        ProductItemDetailResponse productItemDetailResponse =
          productItemResponseMap.get(itemResponse.getItemCode());
        masterDataItemDTO.setGeneratedItemName(productItemDetailResponse.getGeneratedItemName());
        masterDataItemDTO.setDangerousLevel(
          Objects.nonNull(productItemDetailResponse.getDangerousGoodsLevel()) ?
            productItemDetailResponse.getDangerousGoodsLevel() :
            0);
        masterDataItemDTO.setSkuCode(productItemDetailResponse.getSkuCode());
        masterDataItemDTO.setUpcCode(Objects.nonNull(productItemDetailResponse.getUpcCode()) ?
          productItemDetailResponse.getUpcCode() :
          item.getMasterDataItem().getUpcCode());
        masterDataItemDTO.setMasterDataItemImages(
          Optional.ofNullable(productItemDetailResponse.getImages()).orElse(new ArrayList<>())
            .stream().map(image -> convertToMasterDataItemImageDTO(null, image)).collect(toList()));
      }
    }
    return masterDataItemDTO;
  }


  private ProductDetailResponseV2 setProductResponseForView(Product product,
    Map<String, Item> itemMap) throws Exception {
    List<ImageResponse> productImages =
      productCategoryBaseClient.getProductImagesByProductCode(product.getProductCode());
    ProductDetailResponseV2 productDetailResponseV2 = new ProductDetailResponseV2();
    BeanUtils.copyProperties(product,productDetailResponseV2);
    if (CollectionUtils.isNotEmpty(product.getSalesCatalogs())) {
      productDetailResponseV2.setSalesCatalogs(
        product.getSalesCatalogs().stream().map(this::convertToSalesCatalogResponse)
          .collect(toList()));
    }
    if(Objects.nonNull(product.getMasterCatalog())) {
      productDetailResponseV2.setMasterCatalog(
        gdnMapper.deepCopy(product.getMasterCatalog(), MasterCatalogDTO.class));
    }
    MasterDataProductV2 masterDataProductV2 = new MasterDataProductV2();
    masterDataProductV2.setProductName(
      Optional.ofNullable(product.getProductName()).orElse(StringUtils.EMPTY));
    masterDataProductV2.setBrand(product.getBrand());
    masterDataProductV2.setShippingWeight(
      itemMap.values().stream().filter(Objects::nonNull).map(Item::getShippingWeight).findAny()
        .get());
    if(CollectionUtils.isNotEmpty(productImages)) {
      setMasterDataProductImagesForView(productImages, masterDataProductV2);
    }
    productDetailResponseV2.setSuspended(product.isSuspended());
    productDetailResponseV2.setTakenDown(product.isTakenDown());
    productDetailResponseV2.setMasterDataProduct(masterDataProductV2);
    return productDetailResponseV2;
  }

  private void setMasterDataProductImagesForView(List<ImageResponse> productImages,
    MasterDataProductV2 masterDataProductV2) {
    List<MasterDataProductImageDTO> masterDataProductImageDTOS = new ArrayList<>();
    for (ImageResponse imageResponse : productImages) {
      masterDataProductImageDTOS.add(
        gdnMapper.deepCopy(imageResponse, MasterDataProductImageDTO.class));
    }
    masterDataProductV2.setMasterDataProductImages(masterDataProductImageDTOS);
  }

  private MasterDataItemImageDTO convertToMasterDataItemImageDTO(
    MasterDataItemImage masterDataItemImage, Image image) {
    //setting Master Data Item Images from ProductItemResponse if MasterDataItem is null
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    if (Objects.nonNull(masterDataItemImage)) {
      BeanUtils.copyProperties(masterDataItemImage, masterDataItemImageDTO);
    } else {
      masterDataItemImageDTO.setMainImage(image.isMainImages());
      masterDataItemImageDTO.setSequence(image.getSequence());
      masterDataItemImageDTO.setLocationPath(image.getLocationPath());
    }
    return masterDataItemImageDTO;
  }


  private List<MasterDataItemAttributeValueDTO> convertProductAttributeDetailToMasterDataItemAttributeDTO(
    List<ProductAttributeDetail> productAttributeDetails) {
    List<MasterDataItemAttributeValueDTO> masterDataItemAttributes = new ArrayList<>();
    if (CollectionUtils.isEmpty(productAttributeDetails)) {
      return masterDataItemAttributes;
    }
    productAttributeDetails.stream().forEach(productAttributeDetail -> {
      MasterDataItemAttributeValueDTO masterDataItemAttributeValueDTO =
        new MasterDataItemAttributeValueDTO();
      masterDataItemAttributeValueDTO.setAttributeValue(productAttributeDetail.getAttributeValue());
      MasterDataAttributeDTO masterDataAttribute = new MasterDataAttributeDTO();
      masterDataAttribute.setAttributeCode(productAttributeDetail.getAttributeCode());
      masterDataAttribute.setAttributeName(productAttributeDetail.getAttributeName());
      masterDataItemAttributeValueDTO.setMasterDataAttribute(masterDataAttribute);
      masterDataItemAttributes.add(masterDataItemAttributeValueDTO);
    });
    return masterDataItemAttributes;
  }

  private SalesCatalogDTO convertToSalesCatalogResponse(SalesCatalog salesCatalog) {
    return this.gdnMapper.deepCopy(salesCatalog, SalesCatalogDTO.class);
  }

  @Override
  public List<MasterDataProductAttribute> convertToMasterDataProductAttribute(ProductAndAttributeDetailResponse
      productAndAttributeDetailResponse) {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(productAndAttributeDetailResponse, productDetailResponse);
    return convertToMasterDataProduct(productDetailResponse).getMasterDataProductAttributes();
  }

  @Override
  public BasicProductAndItemDTO convertToBasicProductAndItemDTO(Product product,
      List<ItemCatalogVO> itemCatalogVOS, BasicItemDTO item) {
    BasicProductAndItemDTO basicProductAndItemDTO = new BasicProductAndItemDTO();
    basicProductAndItemDTO.setItem(item);
    basicProductAndItemDTO.setProductSku(product.getProductSku());
    basicProductAndItemDTO.setProductCode(product.getProductCode());
    basicProductAndItemDTO.setMerchantCode(product.getMerchantCode());
    basicProductAndItemDTO.setProductType(product.getProductType());
    basicProductAndItemDTO.setPreOrder(product.getPreOrder());
    if (product.isSynchronized()) {
      basicProductAndItemDTO.setMasterDataProduct(new BasicMasterDataProductDTO(product.getBrand(),
          item.getMasterDataItem().getItemDeliveryWeight()));
      basicProductAndItemDTO.setMasterCatalog(new MasterCatalogDTO(masterCatalogCode,
          new CategoryDTO(product.getCategoryCode(), product.getCategoryCode())));
    } else if (Objects.nonNull(product.getMasterDataProduct())) {
      basicProductAndItemDTO.setMasterDataProduct(new BasicMasterDataProductDTO(product.getMasterDataProduct().getBrand(),
          item.getMasterDataItem().getItemDeliveryWeight()));
      if (Objects.nonNull(product.getMasterDataProduct().getMasterCatalog()) &&
          Objects.nonNull(product.getMasterDataProduct().getMasterCatalog().getCategory())) {
        basicProductAndItemDTO.setMasterCatalog(new MasterCatalogDTO(masterCatalogCode,
            new CategoryDTO(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode(),
                product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode())));
      }
    }
    basicProductAndItemDTO.setItemCatalogVOS(itemCatalogVOS);
    setPriceResponseWithValidation(basicProductAndItemDTO.getItem());
    basicProductAndItemDTO.setProductSpecialAttributesList(product.getProductSpecialAttributes());
    return basicProductAndItemDTO;
  }

  private void setPriceResponseWithValidation(BasicItemDTO item) {
    Price price = new Price();
    List<DiscountPriceDTO> discountPriceList =
      item.getPrice().stream().findFirst().map(PriceDTO::getListOfDiscountPrices)
        .orElse(Collections.emptyList());
    if (CollectionUtils.isNotEmpty(discountPriceList)) {
      DiscountPrice discountPrice = itemHelperService.processDiscountPricesByPriority(
        convertToDiscountPriceList(discountPriceList));
      if (Objects.nonNull(discountPrice)) {
        item.getPrice().iterator().next()
            .setListOfDiscountPrices(Collections.singletonList(convertToPrice(discountPrice)));
      }
    }
    if (item.isMerchantPromoDiscount()) {
      DiscountPriceDTO merchantPromoDiscount =
        item.getPrice().stream().findFirst().map(PriceDTO::getMerchantPromoDiscountPrice)
          .orElse(null);
      if (Objects.nonNull(merchantPromoDiscount)) {
        Date now = new Date();
        if (merchantPromoDiscount.getStartDateTime().before(now)
          && merchantPromoDiscount.getEndDateTime().after(now)) {
          item.getPrice().iterator().next().setOfferPrice(merchantPromoDiscount.getDiscountPrice());
          item.getPrice().iterator().next().setMerchantPromoDiscountPrice(merchantPromoDiscount);
        }
      }
    }
  }

  private DiscountPriceDTO convertToPrice(DiscountPrice discountPrice) {
    return this.gdnMapper.deepCopy(discountPrice, DiscountPriceDTO.class);
  }

  private List<DiscountPrice> convertToDiscountPriceList(List<DiscountPriceDTO> discountPriceDTOS){
    List<DiscountPrice> discountPrices = new ArrayList<>();
      discountPriceDTOS.forEach(discountPriceDTO -> {
        DiscountPrice discountPrice = gdnMapper.deepCopy(discountPriceDTO, DiscountPrice.class);
        discountPrices.add(discountPrice);
      });
    return discountPrices;
  }

  @Override
  public ProductScoreVo toProductScoreVoFromProductScore(ProductScore productScore) {
    return gdnMapper.deepCopy(productScore, ProductScoreVo.class);
  }

  @Override
  public CombinedEditItemResponse toCombinedEditItemResponse(EditItemResponse editItemResponse) {
    CombinedEditItemResponse combinedEditItemResponse =
        gdnMapper.deepCopy(editItemResponse, CombinedEditItemResponse.class);
    List<ItemPickupPointVo> updatedItemPickupPointVoList = new ArrayList<>();
    List<ItemPickupPointVo> allUpdatedItemPickupPointVoList = new ArrayList<>();
    List<ItemV2> updatedItems = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(editItemResponse.getUpdatedItemPickupPoints())) {
      for (ItemPickupPoint itemPickupPoint : editItemResponse.getUpdatedItemPickupPoints()) {
        ItemPickupPointVo itemPickupPointVo = gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class);
        updatedItemPickupPointVoList.add(itemPickupPointVo);
      }
      combinedEditItemResponse.setUpdatedItemPickupPointsVo(updatedItemPickupPointVoList);
    }

    if (CollectionUtils.isNotEmpty(editItemResponse.getAllUpdatedItemPickupPoints())) {
      for (ItemPickupPoint itemPickupPoint : editItemResponse.getAllUpdatedItemPickupPoints()) {
        ItemPickupPointVo itemPickupPointVo = gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class);
        allUpdatedItemPickupPointVoList.add(itemPickupPointVo);
      }
      combinedEditItemResponse.setAllUpdatedItemPickupPointsVo(allUpdatedItemPickupPointVoList);
    }

    if (CollectionUtils.isNotEmpty(editItemResponse.getUpdatedItems())) {
      for (Item item : editItemResponse.getUpdatedItems()) {
        ItemV2 itemV2 = gdnMapper.deepCopy(item, ItemV2.class);
        updatedItems.add(itemV2);
      }
      combinedEditItemResponse.setUpdatedItems(updatedItems);
    }
    return combinedEditItemResponse;
  }

  @Override
  public void toSalesCategoryHierarchy(List<ProductBasicResponse> productBasicResponseList,
      Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache) {
    for (ProductBasicResponse productBasicResponse : productBasicResponseList) {
      List<CategoryHierarchyDataResponse> categoryHierarchyDataResponseList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(productBasicResponse.getSalesCategoryCodes())) {
        List<CategoryDataResponse> salesCategoryHierarchy = productBasicResponse.getSalesCategoryCodes().stream()
            .flatMap(categoryCode -> ResponseHelper.toCategoryDataResponse(parentCategoriesFromDbAndCache, categoryCode)
                .stream()).collect(toList());
        productBasicResponse.setSalesCategoryHierarchy(salesCategoryHierarchy);
        for (String categoryCode : productBasicResponse.getSalesCategoryCodes()) {
          CategoryHierarchyDataResponse categoryHierarchyDataResponse = new CategoryHierarchyDataResponse();
          categoryHierarchyDataResponse.setCategoryCode(categoryCode);
          List<CategoryDataResponse> categoryDataResponses =
              ResponseHelper.toCategoryDataResponse(parentCategoriesFromDbAndCache, categoryCode);
          categoryHierarchyDataResponse.setItemCategories(categoryDataResponses);
          categoryHierarchyDataResponseList.add(categoryHierarchyDataResponse);
        }
      }
      productBasicResponse.setSalesCategoryHierarchyV2(categoryHierarchyDataResponseList);
    }
  }

  @Override
  public OdooCreationEventModel convertToOdooCreationEventModel(Product product, List<ItemVo> items) {
    List<ProductItem> productItemList = new ArrayList<>();
    setItemAndPickUpPointsForOdoo(items, productItemList);
    return OdooCreationEventModel.builder().sellerCode(product.getMerchantCode()).productCode(product.getProductCode())
        .productSku(product.getProductSku()).markForDelete(product.isMarkForDelete()).brand(product.getBrand())
        .categoryCode(product.getCategoryCode()).inStore(true).preOrder(
            Optional.ofNullable(Optional.ofNullable(product.getPreOrder()).orElse(new PreOrder()).getIsPreOrder())
                .orElse(false)).imeiRequired(getImeiRequiredFlag(product)).productItems(productItemList).build();
  }

  private boolean getImeiRequiredFlag(Product product) {
    return Optional.ofNullable(product.getProductSpecialAttributes())
      .filter(CollectionUtils::isNotEmpty)
      .map(specialAttributes -> specialAttributes.stream()
        .filter(Objects::nonNull)
        .filter(specialAttribute -> StringUtils.equals(specialAttribute.getAttributeCode(), imeiAttributeCode))
        .map(ProductSpecialAttribute::getAttributeValue)
        .findFirst()
        .map(imeiValue -> imeiAllowedValues.contains(imeiValue))
        .orElse(false))
      .orElse(false);
  }

  private static void setItemAndPickUpPointsForOdoo(List<ItemVo> items, List<ProductItem> productItemList) {
    if (CollectionUtils.isNotEmpty(items)) {
      for (ItemVo itemVo : items) {
        ProductItem productItem = new ProductItem();
        productItem.setItemSku(itemVo.getItemSku());
        productItem.setItemName(itemVo.getGeneratedItemName());
        productItem.setSellerSku(itemVo.getMerchantSku());
        productItem.setUpcCode(itemVo.getUpcCode());
        List<OdooPickupPointModel> pickupPointList = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(itemVo.getItemPickupPointVoList())) {
          for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
            OdooPickupPointModel pickupPoint = new OdooPickupPointModel();
            pickupPoint.setPickupPointCode(itemPickupPointVo.getPickupPointCode());
            if(CollectionUtils.isNotEmpty(itemPickupPointVo.getPrice())){
              pickupPoint.setPrice(Optional.ofNullable(itemPickupPointVo.getPrice()).orElse(new HashSet<>()).stream()
                  .filter(Objects::nonNull).findFirst().get().getListPrice());
            }
            pickupPointList.add(pickupPoint);
          }
          productItem.setPickupPoints(pickupPointList);
          productItemList.add(productItem);
        }
      }
    }
  }
}
