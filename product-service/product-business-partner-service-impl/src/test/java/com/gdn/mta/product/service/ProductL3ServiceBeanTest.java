package com.gdn.mta.product.service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductCodeAndSkuRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import com.gdn.GdnBaseEntity;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductItemsCogs;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.BusinessPartnerRepositoryBean;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryResponseV2DTO;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutboundBean;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.DistributionInfoDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductScoreResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Predicate;

import static com.gdn.mta.product.util.GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI;
import static com.gdn.mta.product.util.GdnBaseLookup.PRODUCT_CODE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ProductL3ServiceBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_SKU_CODE = "MTA-0000001-00001";
  private static final String DEFAULT_SKU_CODE_2 = "MTA-0000001-00002";
  private static final String DEFAULT_SKU_CODE_3 = "MTA-0000001-00003";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_SETTLEMENT_TYPE = "REGULAR";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001-00001";
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String DEFAULT_FBB_PICKUP_POINT_CODE = "PP-0000002";
  private static final String ITEM_SKU = "TOT-15014-0001-0001";
  private static final String DEFAULT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_ATTRIBUTE_CODE = "AT-0000001";
  private static final String DEFAULT_ATTRIBUTE_CODE_2 = "AT-0000002";
  private static final String DEFAULT_ATTRIBUTE_CODE_3 = "AT-0000003";
  private static final String DEFAULT_ATTRIBUTE_CODE_4 = "AT-0000004";
  private static final String DEFAULT_ATTRIBUTE_CODE_6 = "AT-0000006";
  private static final String DEFAULT_BRAND = "TEST";
  private static final String DEFAULT_DESCRIPTION = "TEST";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String DEFAULT_ATTRIBUTE_TYPE =
    MasterDataAttributeType.DEFINING_ATTRIBUTE.name();
  private static final String DEFAULT_ITEM_SKU = "SKU-1000";
  private static final String DEFAULT_ITEM_SKU_1 = "SKU-1000-1";
  private static final String DEFAULT_LOCATION_PATH = "TEST";
  private static final String DEFAULT_ATTRIBUTE_TYPE_2 =
    MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.name();
  private static final String DEFAULT_ATTRIBUTE_TYPE_3 =
    MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name();
  private static final String MAIN_IMAGE_URL = "MAIN_IMAGE_URL";
  private static final String RESIZE_IMAGE_URL = "resize/";
  private MandatoryRequestParam mandatoryRequestParam;
  private ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO>
      webInventoryUpdatePickupPointRequestDTOListRequestDTO;
  private WebInventoryUpdatePickupPointResponseDTO updatePickupPointResponse;

  private static final String STOREID = "storeId";
  private static final String REQUESTID = "requestId";
  private static final String PICKUP = "PICKUP";
  private static final String PICKUP_1 = "PICKUP_1";
  private static final String PRODUCT_NAME1 = "PRODUCT_NAME1";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String PRODUCT_DESCRIPTION1 = "PRODUCT_DESCRIPTION1";
  private static final String PRODUCT_USP1 = "PRODUCT_USP1";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String DEFAULT_STORE_ID = "10001";
  public static final String ITEM_NAME = "itemName";
  private static final String DEFAULT_ORIGIN_ID = "originId";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "productBusinessPartnerId";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ITEM_ID_2 = "productItemId2";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final double PRICE = 1000.0;
  private static final double SALE_PRICE = 2000.0;
  private static final int AVAILABLE_STOCK_2 = 1;
  private static final int AVAILABLE_STOCK_1 = 2;
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String DELETED = "DELETED";
  private static final String NEED_CORRECTION = "NEED_CORRECTION";
  private static final String ACTIVE = "ACTIVE";
  private static final String CHANNEL_ID = "CHANNEL_ID";
  private static final String CLIENT_ID = "CLIENT_ID";
  private static final String SUSPENDED = "Suspended";

  private ProductCollection productCollection;
  private Image image;
  private List<CategoryResponse> categoriesData;
  private PreOrderDTO preOrderDTO;
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
  ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
  private AttributeResponse attribute;
  private ItemPickupPointListingL3Request itemPickupPointListingL3Request;
  private ItemPickupPointListingResponse itemPickupPointListingResponse;
  private ProductItemResponse productItemResponse;
  private ItemImageResponse itemImageResponse;
  private ProductLevel3Inventory productLevel3Inventory;
  private CampaignPriceResponse campaignPriceResponse;
  private ProductItemWholesalePrice productItemWholesalePrice;
  private ProductItemWholesalePriceResponse productItemWholesalePriceResponse;
  private ProductItemsCogs productItemsCogs;
  private ItemResponseV2 itemResponseV2 = new ItemResponseV2();
  private PriceResponse priceResponse = new PriceResponse();
  private ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
  private ItemPickupPointSummaryRequest itemPickupPointSummaryRequest =
    ItemPickupPointSummaryRequest.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .productSkuList(Collections.singletonList(DEFAULT_PRODUCT_SKU)).build();
  private WholesalePriceSkuResponse wholesalePriceSkuResponse;
  private ItemSkuPickupPointRequest itemSkuPickupPointRequest;
  private DeleteItemPickupPointRequest deleteItemPickupPointRequest;
  private DeleteItemPickupPointResponse deleteItemPickupPointResponse;
  private List<DeleteItemPickupPointResponse> responseList;
  private PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
  List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
  PickupPointResponse pickupPointResponse = new PickupPointResponse();
  ProductCodeAndSkuRequest productCodeAndSkuRequest = new ProductCodeAndSkuRequest();
  ProductCenterDetailResponse productCenterDetailResponse = new ProductCenterDetailResponse();

  @InjectMocks
  private ProductL3ServiceBean productL3ServiceBean;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private InventoryOutboundBean inventoryOutboundBean;

  @Mock
  private InventoryFeign inventoryFeign;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private CampaignOutbound campaignOutbound;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private MapperUtil mapperUtil;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private BusinessPartnerRepositoryBean businessPartnerRepositoryBean;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ConverterUtil converterUtil;

  @Mock
  private PreOrderConfig preOrderConfig;

  @Captor
  private ArgumentCaptor<CategoryCodeRequest> categoryCodeRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<SkuCodesRequest> skuCodesRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemBusinessPartner>> productItemBusinessPartnerListCaptor;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    CategoryResponse categoryResponse1 =
      new CategoryResponse("C2", DEFAULT_CATEGORY_CODE, 0, null, "".getBytes(), "".getBytes(), null,
        true, 100, true, false, true, new CatalogResponse(), null);
    categoryResponse1.setId("id1");

    CategoryResponse categoryResponse2 =
      new CategoryResponse("C1", null, 0, null, "".getBytes(), "".getBytes(), null, true, 100, true,
        false, true, new CatalogResponse(), null);
    categoryResponse2.setId("id2");

    categoriesData = new ArrayList<CategoryResponse>();
    categoriesData.add(categoryResponse1);
    categoriesData.add(categoryResponse2);

    preOrderDTO = PreOrderDTO.builder().isPreOrder(true).preOrderType(PREORDER_TYPE)
      .preOrderValue(PREORDER_VALUE).build();

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);

    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setPrice(100.0);
    productItemBusinessPartner.setSalePrice(90.0);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    productItemBusinessPartner.setProductType(1);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner1.setPrice(100.0);
    productItemBusinessPartner1.setSalePrice(90.0);
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);
    productItemBusinessPartner1.setProductType(1);
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setId(PRODUCT_BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductId(PRODUCT_ID);

    productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);

    image = new Image();
    image.setMainImages(true);
    image.setLocationPath(DEFAULT_LOCATION_PATH);
    image.setSequence(1);

    attribute = new AttributeResponse();
    attribute.setId(DEFAULT_ATTRIBUTE_ID);
    attribute.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);

    itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    itemPickupPointListingL3Request.setProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);

    itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    itemPickupPointListingResponse.setItemSku(DEFAULT_ITEM_SKU);
    itemPickupPointListingResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    itemPickupPointListingResponse.setSkuCode(DEFAULT_SKU_CODE);
    itemPickupPointListingResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);
    itemPickupPointListingResponse.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);
    itemPickupPointListingResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    itemPickupPointListingResponse.setProductType(ProductType.REGULAR);
    itemPickupPointListingResponse.setPrices(new ArrayList<>());
    itemPickupPointListingResponse.setViewConfigs(new ArrayList<>());

    productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(DEFAULT_SKU_CODE);
    productItemResponse.setImages(Arrays.asList(image));
    productItemResponse.setId(PRODUCT_ITEM_ID);

    pickupPointFilterRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setFbbActivated(false);
    pickupPointResponse.setCode("pickuppoint");
    pickupPointResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponseList.add(pickupPointResponse);
    webInventoryUpdatePickupPointRequestDTOListRequestDTO = new ListRequestDTO<>();

    ImageResponse imageResponse = new ImageResponse();
    BeanUtils.copyProperties(image, imageResponse);
    imageResponse.setMainImage(image.isMainImages());

    itemImageResponse = new ItemImageResponse();
    itemImageResponse.setItemCode(DEFAULT_SKU_CODE);
    itemImageResponse.setImageResponses(Arrays.asList(imageResponse));

    productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setWebItemSku(DEFAULT_ITEM_SKU);
    productLevel3Inventory.setWebPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    productLevel3Inventory.setWebAvailable(AVAILABLE_STOCK_2);
    productLevel3Inventory.setWarehouseAvailable(AVAILABLE_STOCK_1);
    productLevel3Inventory.setWebSyncStock(true);

    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    campaignPriceSkuResponse.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);
    campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemInfoToPriceResponse(Arrays.asList(campaignPriceSkuResponse));

    productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(ITEM_SKU);
    productItemWholesalePrice.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);

    productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();

    productItemsCogs = new ProductItemsCogs();

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(eq(DEFAULT_CATEGORY_CODE)))
      .thenReturn(categoriesData);
    itemResponseV2.setProductSku(DEFAULT_PRODUCT_SKU);
    itemResponseV2.setItemSku(DEFAULT_ITEM_SKU);
    itemResponseV2.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);
    priceResponse.setPrice(PRICE);
    priceResponse.setSalePrice(SALE_PRICE);
    viewConfigResponse.setBuyable(true);
    viewConfigResponse.setDisplay(true);
    itemResponseV2.setPrices(Collections.singletonList(priceResponse));
    itemResponseV2.setViewConfigs(Collections.singletonList(viewConfigResponse));

    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);

    wholesalePriceSkuResponse = new WholesalePriceSkuResponse();
    wholesalePriceSkuResponse.setItemSku(DEFAULT_ITEM_SKU);
    wholesalePriceSkuResponse.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);
    wholesalePriceSkuResponse.setWholesaleRules(new HashMap<>());

    itemSkuPickupPointRequest = new ItemSkuPickupPointRequest();
    itemSkuPickupPointRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    itemSkuPickupPointRequest.setItemSkuList(Collections.singletonList(DEFAULT_ITEM_SKU));
    itemSkuPickupPointRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    itemSkuPickupPointRequest.setPickupPointCode(PICKUP);

    deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    deleteItemPickupPointRequest.setItemSkus(Collections.singletonList(DEFAULT_ITEM_SKU));
    deleteItemPickupPointRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP);

    deleteItemPickupPointResponse = new DeleteItemPickupPointResponse();
    deleteItemPickupPointResponse.setItemSku(DEFAULT_ITEM_SKU);
    responseList = new ArrayList<>();
    responseList.add(deleteItemPickupPointResponse);

    ReflectionTestUtils.setField(productL3ServiceBean, "fbbSortingSellers", "CC,TD,TC");
    ReflectionTestUtils.setField(productL3ServiceBean, "autoHealMainImageUrlEnabled", false);
    ReflectionTestUtils.setField(productL3ServiceBean, "sizeChartValueTypeDelimiter", "-");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productRepository);
  }


  @Test
  public void getL3DetailByProductSkuTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getMasterDataProduct()
      .setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
      .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, true));
    savedProductData.setPreOrderDTO(preOrderDTO);
    savedProductData.setActiveL5Mapped(true);
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertTrue(productL3DetailsResponse.isActiveL5Mapped());
  }

  @Test
  public void getL3DetailByProductSkuProductTypeNullTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getMasterDataProduct()
        .setMasterDataProductImages(new ArrayList<>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
        .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, true));
    savedProductData.setPreOrderDTO(preOrderDTO);
    savedProductData.setActiveL5Mapped(true);
    savedProductData.setProductType(null);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertTrue(productL3DetailsResponse.isActiveL5Mapped());
  }

  @Test
  public void getL3DetailByProductSkuProductTypeNull2Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "setDefaultProductType", true);
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getMasterDataProduct()
        .setMasterDataProductImages(new ArrayList<>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
        .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, true));
    savedProductData.setPreOrderDTO(preOrderDTO);
    savedProductData.setActiveL5Mapped(true);
    savedProductData.setProductType(null);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertTrue(productL3DetailsResponse.isActiveL5Mapped());
    Assertions.assertEquals(1, productL3DetailsResponse.getProductType(), 0);
  }

  @Test
  public void getL3DetailByProductSku_withNullMerchantDeliverTypeTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryRestrictionFeatureEnabled",true);
    ReflectionTestUtils.setField(productL3ServiceBean, "productSuitabilityFeatureEnabled", true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setMarkForDelete(true);
    savedProductData.setMasterDataProduct(null);
    savedProductData.setDefiningAttributes(null);
    savedProductData.setDescriptiveAttributes(null);
    savedProductData.setProductSpecialAttributes(null);
    savedProductData.setProductScore(null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantDeliveryType(null);
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(profileResponse);
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
  }

  @Test
  public void getL3DetailByProductSku_AiGeneratedFieldsTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes",
        "CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryRestrictionFeatureEnabled",
        true);
    ReflectionTestUtils.setField(productL3ServiceBean, "productSuitabilityFeatureEnabled", true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setMarkForDelete(true);
    savedProductData.setMasterDataProduct(null);
    savedProductData.setDefiningAttributes(null);
    savedProductData.setDescriptiveAttributes(null);
    savedProductData.setProductSpecialAttributes(null);
    savedProductData.setProductScore(null);
    com.gdn.x.product.model.vo.AiGeneratedFieldsResponse aiGeneratedFieldsResponse =
        new com.gdn.x.product.model.vo.AiGeneratedFieldsResponse();
    aiGeneratedFieldsResponse.setAiGeneratedBrand(true);
    aiGeneratedFieldsResponse.setAiGeneratedCategory(true);
    savedProductData.setAiGeneratedFieldsResponse(aiGeneratedFieldsResponse);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantDeliveryType(null);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(profileResponse);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
        DEFAULT_PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    ProductL3DetailsResponse productL3DetailsResponse =
        productL3ServiceBean.getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU,
            false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(
        DEFAULT_BUSINESS_PARTNER_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(true,
        productL3DetailsResponse.getAiGeneratedFieldsResponse().isAiGeneratedBrand());
    Assertions.assertEquals(true,
        productL3DetailsResponse.getAiGeneratedFieldsResponse().isAiGeneratedCategory());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionTest() throws Exception {
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
      .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
      .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionDistributionInfoTest() throws Exception {
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    DistributionInfoResponse distributionInfoResponse = new DistributionInfoResponse();
    distributionInfoResponse.setProductName(PRODUCT_NAME1);
    distributionInfoResponse.setCategoryName(CATEGORY_NAME);
    productDetailResponse.setDistributionInfoResponse(distributionInfoResponse);
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(PRODUCT_NAME1, productL3DetailsResponse.getDistributionInfoResponse().getProductName());
    Assertions.assertEquals(CATEGORY_NAME, productL3DetailsResponse.getDistributionInfoResponse().getCategoryName());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionBfBTest() throws Exception {
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setB2cActivated(true);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(productL3DetailsResponse.isB2cActivated(), true);
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionFbbTrueTest() throws Exception {
    productItemBusinessPartner1.setPickupPointId(DEFAULT_FBB_PICKUP_POINT_CODE);
    productItemBusinessPartner1.setFbbActive(true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner,productItemBusinessPartner1));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(productBusinessPartner.getProductItemBusinessPartners().size(),2);
    Assertions.assertEquals(productL3DetailsResponse.getPickupPointCodes().get(0),DEFAULT_PICKUP_POINT_CODE);
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionFbbTest() throws Exception {
    productItemBusinessPartner.setMarkForDelete(false);
    productItemBusinessPartner.setFbbActive(true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionFbbFalseTest() throws Exception {
    productItemBusinessPartner.setMarkForDelete(false);
    productItemBusinessPartner.setFbbActive(false);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuNeedRevisionFbbFalseMfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchL4BasedOnMfdInNrFlow", true);
    productItemBusinessPartner.setMarkForDelete(true);
    productItemBusinessPartner.setFbbActive(false);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setSizeChartCode(SIZE_CHART_CODE);
    image.setCommonImage(false);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setAiGeneratedFieldsResponse(new AiGeneratedFieldsResponse(true, false));
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(profileResponse);
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(SIZE_CHART_CODE, productL3DetailsResponse.getSizeChartCode());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(0, productL3DetailsResponse.getItemCount());
    Assertions.assertTrue(productL3DetailsResponse.getAiGeneratedFieldsResponse().isAiGeneratedCategory());
    Assertions.assertFalse(productL3DetailsResponse.getAiGeneratedFieldsResponse().isAiGeneratedBrand());
  }

  @Test
  public void getL3DetailByProductSkuDataNeedRevisionTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","C");
    ReflectionTestUtils.setField(productL3ServiceBean,"ranchIntegrationEnabled",true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
      .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setDistributionInfoResponse(new DistributionInfoResponse());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
      new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
      .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    AllowedAttributeValueResponse allowedAttributeValueResponse1 = new AllowedAttributeValueResponse();
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse1);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
      new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
      new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
      new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    allowedAttributeValueResponse.setValueType("UK");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);

    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
      new ProductAttributeValueResponse();
    productAttributeResponse3.setAttribute(attributeResponse3);
    productAttributeResponse3.getProductAttributeValues().add(productAttributeValueResponse3);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse3);

    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
      .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(4, productL3DetailsResponse.getAttributes().size());
    Assertions.assertEquals(10.0, productL3DetailsResponse.getLength(), 0.0);
  }

  @Test
  public void getL3DetailByProductSkuDataNeedRevisionTest4() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
        new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
        new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
        new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    allowedAttributeValueResponse.setValueType("US");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);

    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
        new ProductAttributeValueResponse();
    productAttributeResponse3.setAttribute(attributeResponse3);
    productAttributeResponse3.getProductAttributeValues().add(productAttributeValueResponse3);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse3);

    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(4, productL3DetailsResponse.getAttributes().size());
    Assertions.assertEquals(10.0, productL3DetailsResponse.getLength(), 0.0);
  }

  @Test
  public void getL3DetailByProductSkuDataNeedRevisionTest2() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
        new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
        new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
        new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);

    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
        new ProductAttributeValueResponse();
    productAttributeResponse3.setAttribute(attributeResponse3);
    productAttributeResponse3.getProductAttributeValues().add(productAttributeValueResponse3);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse3);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(4, productL3DetailsResponse.getAttributes().size());
    Assertions.assertEquals(10.0, productL3DetailsResponse.getLength(), 0.0);
  }

  @Test
  public void getL3DetailByProductSkuCategoryNotEligibleForBopisProductIsBopisTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
        new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
        new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
        new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
        new ProductAttributeValueResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setBopisEligible(true);
    categoryResponse.setActivated(true);
    Mockito.when(categoryRepository.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryResponse));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuCategoryDimensionMissingTrueTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryRestrictionFeatureEnabled",true);
    ReflectionTestUtils.setField(productL3ServiceBean,"productSuitabilityFeatureEnabled",true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setProductType(ProductType.BOPIS.getCode());
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    attributeResponse.setHideForSeller(true);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
        new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    attributeResponse2.setDsExtraction(true);
    ProductAttributeValueResponse productAttributeValueResponse2 =
        new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
        new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
        new ProductAttributeValueResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setBopisEligible(false);
    categoryResponse.setMarkForDelete(true);
    categoryResponse.setActivated(true);
    Mockito.when(categoryRepository.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE))
        .thenReturn(Collections.singletonList(categoryResponse));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());

    boolean dsAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isDsExtraction);
    boolean hiddenAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isHideFromSeller);
    Assertions.assertFalse(hiddenAttributePresent);
    Assertions.assertTrue(dsAttributePresent);
  }

  @Test
  public void getL3DetailByProductSkuCategoryDimensionMissingTrueRegularTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryRestrictionFeatureEnabled",true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setProductType(ProductType.REGULAR.getCode());
    productBusinessPartner
      .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
      new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
      .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
      new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
      new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
      new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
      new ProductAttributeValueResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setBopisEligible(false);
    categoryResponse.setMarkForDelete(true);
    categoryResponse.setActivated(true);
    Mockito.when(categoryRepository.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE))
      .thenReturn(Collections.singletonList(categoryResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
      .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuCategoryDimensionMissingTrueBopisFalseTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryRestrictionFeatureEnabled",true);
    ReflectionTestUtils.setField(productL3ServiceBean,"ranchIntegrationEnabled",true);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setProductType(ProductType.BOPIS.getCode());
    productBusinessPartner
      .setProductItemBusinessPartners(List.of(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse =
      new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse
      .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 =
      new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 =
      new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
      new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 =
      new ProductAttributeValueResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setBopisEligible(true);
    categoryResponse.setMarkForDelete(true);
    categoryResponse.setActivated(true);
    Mockito.when(categoryRepository.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE))
      .thenReturn(Collections.singletonList(categoryResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService
        .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
      .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, true);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository)
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }

  @Test
  public void getL3DetailByProductSkuDataNeedRevisionTest3() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean,"bopisCategoryValidationMerchantTypes","CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    image.setCommonImage(true);
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setPreOrderDTO(preOrderDTO);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setPreOrderValue(10);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    productDetailResponse.setImages(Arrays.asList(image));
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse.setName("Color");
    attributeResponse.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("Orange");
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.getProductAttributeValues().add(productAttributeValueResponse);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_2);
    attributeResponse1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.setName("RAM");
    attributeResponse1.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    productAttributeValueResponse1.setDescriptiveAttributeValue("10GB");
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.getProductAttributeValues().add(productAttributeValueResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.setName("OS");
    attributeResponse2.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue("Windows");
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.getProductAttributeValues().add(productAttributeValueResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);

    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setProductAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeType(DEFAULT_ATTRIBUTE_TYPE_3);
    attributeResponse3.setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse3.setName("OS");
    attributeResponse3.setSkuValue(false);
    ProductAttributeValueResponse productAttributeValueResponse3 = new ProductAttributeValueResponse();
    productAttributeResponse3.setAttribute(attributeResponse3);
    productAttributeResponse3.getProductAttributeValues().add(productAttributeValueResponse3);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse3);

    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.when(productCollectionRepository.getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(DEFAULT_PRODUCT_CODE);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(new ArrayList<>());
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false))
        .thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse =
        productL3ServiceBean.getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, true, true, false);
    verify(productCollectionRepository).getProductCodeByGdnSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, true, false);
    verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    verify(productCollectionRepository).findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    Assertions.assertEquals(4, productL3DetailsResponse.getAttributes().size());
    Assertions.assertEquals(10.0, productL3DetailsResponse.getLength(), 0.0);
  }

  @Test
  public void getL3DetailByProductSkuDifferentAttributeTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "productSuitabilityFeatureEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "ranchIntegrationEnabled", true);
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3).getMasterDataAttribute().setHideFromSeller(true);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(2).getMasterDataAttribute().setDsExtraction(true);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getProductSpecialAttributes().get(0)
      .setAttributeCode(DEFAULT_ATTRIBUTE_CODE_6);
    savedProductData.getMasterDataProduct()
      .setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
      .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, false));
    savedProductData.setPreOrderDTO(preOrderDTO);
    when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_6))
      .thenReturn(attribute);
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    savedProductData.getDefiningAttributes().add(savedProductData.getDefiningAttributes().get(0));
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(3, productL3DetailsResponse.getAttributes().size(), 0);
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    boolean dsAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isDsExtraction);
    boolean hiddenAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isHideFromSeller);
    Assertions.assertFalse(hiddenAttributePresent);
    Assertions.assertTrue(dsAttributePresent);
  }

  @Test
  public void getL3DetailByProductSkuDifferentAttributeNonNullTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ReflectionTestUtils.setField(productL3ServiceBean, "productSuitabilityFeatureEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "ranchIntegrationEnabled", true);
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3).getMasterDataAttribute().setHideFromSeller(true);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(2).getMasterDataAttribute().setDsExtraction(true);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getProductSpecialAttributes().get(0)
        .setAttributeCode(DEFAULT_ATTRIBUTE_CODE_6);
    savedProductData.getMasterDataProduct()
        .setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
        .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, false));
    savedProductData.setPreOrderDTO(preOrderDTO);
    when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_6))
        .thenReturn(attribute);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    savedProductData.getDefiningAttributes().add(savedProductData.getDefiningAttributes().get(0));
    DistributionInfoDTO distributionInfoDTO = new DistributionInfoDTO();
    distributionInfoDTO.setProductName(PRODUCT_NAME1);
    savedProductData.setDistributionInfoDTO(distributionInfoDTO);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
            .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
        .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
            .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
        .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(3, productL3DetailsResponse.getAttributes().size(), 0);
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
    boolean dsAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isDsExtraction);
    boolean hiddenAttributePresent = productL3DetailsResponse.getAttributes().stream()
        .anyMatch(ProductLevel3AttributeResponse::isHideFromSeller);
    Assertions.assertFalse(hiddenAttributePresent);
    Assertions.assertTrue(dsAttributePresent);
  }

  @Test
  public void getL3DetailByProductSku_suspendedProductEmptyCategorieTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "CM");
    ProductL3Response savedProductData = generateProductL3Response();
    savedProductData.setMarkForDelete(true);
    savedProductData.setMasterDataProduct(null);
    savedProductData.setDefiningAttributes(null);
    savedProductData.setDescriptiveAttributes(null);
    savedProductData.setProductSpecialAttributes(null);
    savedProductData.setProductScore(null);
    savedProductData.setSuspended(true);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantDeliveryType(null);
    Mockito.when(categoryRepository.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE))
      .thenReturn(new ArrayList<>());
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(profileResponse);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
  }

  @Test
  public void getL3DetailByProductSku_DifferentAttributesTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "bopisCategoryValidationMerchantTypes", "C");
    ProductL3Response savedProductData = generateProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueDTO = new PredefinedAllowedAttributeValueDTO();
    predefinedAllowedAttributeValueDTO.setPredefinedAllowedAttributeCode("brandCode");
    masterDataProductAttributeValueDTO.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueDTO);
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
        .setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValueDTO));
    savedProductData.getMasterDataProduct().getMasterDataProductAttributes().get(3).getMasterDataAttribute()
        .setAttributeName(Constants.BRAND);
    savedProductData.getProductSpecialAttributes().get(0)
      .setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    savedProductData.getMasterDataProduct()
      .setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    savedProductData.getMasterDataProduct().getMasterDataProductImages()
      .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0, false));
    savedProductData.setPreOrderDTO(preOrderDTO);
    when(productOutbound.getAttributeDetailByAttributeCode(DEFAULT_ATTRIBUTE_CODE_6))
      .thenReturn(attribute);
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(new GdnRestSingleResponse<>(savedProductData, REQUESTID));
    Mockito.when(productLevel3LogisticsService
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP))
      .thenReturn(Arrays.asList(new ProductLevel3Logistics()));
    Mockito.when(this.productCollectionRepository
      .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
      .thenReturn(productCollection);
    ProductL3DetailsResponse productL3DetailsResponse = productL3ServiceBean
      .getL3ProductDetailsByProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, false, true, false);
    verify(xProductOutbound).getProductDetailsByProductSku(DEFAULT_PRODUCT_SKU);
    verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    verify(productLevel3LogisticsService)
      .findLogisticsByItemSku(DEFAULT_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE, PICKUP);
    verify(categoryRepository).findHierarchyByCategoryCode(Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, productL3DetailsResponse.getProductSku());
    Assertions.assertEquals(PREORDER_VALUE, productL3DetailsResponse.getPreOrder().getPreOrderValue());
  }


  private ProductDetailResponse generateProductDetailResponse() {
    ProductDetailResponse product = new ProductDetailResponse();
    product.setId(UUID.randomUUID().toString());
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    product.setProductItemResponses(new HashSet<ProductItemResponse>());
    product.setStoreId(STOREID);
    ProductItemResponse productItem = new ProductItemResponse();
    productItem.setId(UUID.randomUUID().toString());
    productItem.setSkuCode(DEFAULT_SKU_CODE);
    ProductItemResponse productItem2 = new ProductItemResponse();
    productItem2.setId(UUID.randomUUID().toString());
    productItem2.setSkuCode(DEFAULT_SKU_CODE_2);
    ProductItemResponse productItem3 = new ProductItemResponse();
    productItem3.setId(UUID.randomUUID().toString());
    productItem3.setSkuCode(DEFAULT_SKU_CODE_3);
    product.getProductItemResponses().add(productItem);
    product.getProductItemResponses().add(productItem2);
    product.getProductItemResponses().add(productItem3);
    return product;
  }

  private ProfileResponse getProfileResponse() {
    ProfileResponse businessPartner = new ProfileResponse();
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointDTO.setOriginId(DEFAULT_ORIGIN_ID);
    businessPartner.setPickupPoints(Arrays.asList(pickupPointDTO));
    CompanyDTO dto = new CompanyDTO();
    dto.setInternationalFlag(Boolean.TRUE);
    businessPartner.setCompany(dto);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.getCompany().setMerchantDeliveryType(PICKUP);
    businessPartner.getCompany().setInventoryFulfillment(INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setMerchantType("CM");
    return businessPartner;
  }

  private ProductL3Response generateProductL3Response() throws Exception {
    ProductL3Response productData = new ProductL3Response();
    productData.setVersion(10L);
    productData.setItemSkus(Arrays.asList(DEFAULT_ITEM_SKU));
    productData.setProductEditable(true);
    productData.setProductSku(DEFAULT_PRODUCT_SKU);
    productData.setProductCode(DEFAULT_PRODUCT_CODE);
    productData.setProductType(ProductType.REGULAR);
    productData.setSettlementType(DEFAULT_SETTLEMENT_TYPE);
    productData.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productData.setSynchronized(true);
    productData.setDefaultItemSku(DEFAULT_ITEM_SKU);
    productData.setMasterCatalog(new MasterCatalogDTO());
    productData.getMasterCatalog().setCategory(new CategoryDTO());
    productData.getMasterCatalog().getCategory().setCategoryCode(DEFAULT_CATEGORY_CODE);
    productData.setMasterDataProduct(new MasterDataProductDTO());
    productData.getMasterDataProduct().setDescription(PRODUCT_DESCRIPTION1);
    productData.getMasterDataProduct().setProductName(PRODUCT_NAME1);
    productData.getMasterDataProduct().setUniqueSellingPoint(PRODUCT_USP1);
    productData.getMasterDataProduct().setBrand(DEFAULT_BRAND);
    productData.getMasterDataProduct()
      .setMasterDataProductAttributes(new ArrayList<MasterDataProductAttributeDTO>());
    productData.getMasterDataProduct().getMasterDataProductAttributes()
      .add(new MasterDataProductAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes()
      .add(new MasterDataProductAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes()
      .add(new MasterDataProductAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes()
      .add(new MasterDataProductAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .setMasterDataAttribute(new MasterDataAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setVariantCreation(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setBasicView(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setMandatory(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataAttribute().setSkuValue(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .setMasterDataProductAttributeValues(new ArrayList<MasterDataProductAttributeValueDTO>());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(0)
      .getMasterDataProductAttributeValues().add(new MasterDataProductAttributeValueDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .setMasterDataAttribute(new MasterDataAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setBasicView(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setMandatory(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setSkuValue(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .setMasterDataProductAttributeValues(new ArrayList<MasterDataProductAttributeValueDTO>());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(1)
      .getMasterDataAttribute().setSkuValue(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .setMasterDataAttribute(new MasterDataAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataAttribute().setBasicView(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataAttribute().setMandatory(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataAttribute().setSkuValue(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .setMasterDataProductAttributeValues(new ArrayList<MasterDataProductAttributeValueDTO>());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(2)
      .getMasterDataProductAttributeValues().add(new MasterDataProductAttributeValueDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .setMasterDataAttribute(new MasterDataAttributeDTO());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataAttribute().setBasicView(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataAttribute().setMandatory(true);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataAttribute().setSkuValue(false);
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .setMasterDataProductAttributeValues(new ArrayList<MasterDataProductAttributeValueDTO>());
    productData.getMasterDataProduct().getMasterDataProductAttributes().get(3)
      .getMasterDataProductAttributeValues().add(new MasterDataProductAttributeValueDTO());
    productData.setProductSpecialAttributes(new ArrayList<ProductSpecialAttributeDTO>());
    productData.getProductSpecialAttributes().add(new ProductSpecialAttributeDTO());
    productData.getProductSpecialAttributes().get(0).setAttributeCode(DEFAULT_ATTRIBUTE_CODE_4);
    productData.setDescriptiveAttributes(new ArrayList<ProductAttributeDetailDTO>());
    productData.getDescriptiveAttributes().add(new ProductAttributeDetailDTO());
    productData.getDescriptiveAttributes().add(new ProductAttributeDetailDTO());
    productData.getDescriptiveAttributes().get(0).setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    productData.getDescriptiveAttributes().get(1).setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    productData.setDefiningAttributes(new ArrayList<ProductAttributeDTO>());
    productData.getDefiningAttributes().add(new ProductAttributeDTO());
    productData.getDefiningAttributes().get(0)
      .setProductAttributeDetails(new ArrayList<ProductAttributeDetailDTO>());
    productData.getDefiningAttributes().get(0).getProductAttributeDetails()
      .add(new ProductAttributeDetailDTO());
    productData.getDefiningAttributes().get(0).getProductAttributeDetails().get(0)
      .setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    productData.getMasterDataProduct()
      .setMasterDataProductImages(new ArrayList<MasterDataProductImageDTO>());
    productData.getMasterDataProduct().getMasterDataProductImages()
      .add(new MasterDataProductImageDTO(true, null, DEFAULT_PRODUCT_CODE, 0));
    productData.setForceReview(true);
    productData
      .setProductScore(new ProductScoreResponse(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    productData.setPickupPointCodes(Arrays.asList(PICKUP, PICKUP_1));
    return productData;
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", false);
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, true, true);
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
    Mockito
      .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsConcatValueTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", true);
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, true, true);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
    Mockito
        .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(productOutbound).getProductAttributesByProductId(productCollection.getProductId());
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsInvSkipTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "valueTypeAdditionForDefiningAttributes", true);
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, true, false);
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
    Mockito
        .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(productOutbound).getProductAttributesByProductId(productCollection.getProductId());
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsFbbTrueTest() throws Exception {
    itemPickupPointListingResponse.setFbbActive(true);
    itemPickupPointListingResponse.setFbbActiveAtL3Level(true);
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
        eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
        productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage =
        productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
    Assertions.assertTrue(itemPickupPointListingL3ResponsePage.getContent().get(0).isFbbActivated());
    Mockito
      .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsTestPricingMpp() throws Exception {
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
        eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(categoryRepository.findHierarchyByCategoryCodes(categoryCodeRequestArgumentCaptor.capture()))
        .thenReturn(Arrays.asList(
            new CategoryHierarchyResponse(DEFAULT_CATEGORY_CODE, DEFAULT_CATEGORY_CODE, 1, categoriesData)));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequestArgumentCaptor.capture()))
        .thenReturn(Arrays.asList(productItemResponse));
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
        productLevel3Service.getProductItemCogsValues(DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_SKU_CODE)))
        .thenReturn(ImmutableMap.of(DEFAULT_SKU_CODE, productItemsCogs));
    Mockito.when(
        productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito.when(
            productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));

    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(),false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsTestPricingMppAutoHealTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "autoHealMainImageUrlEnabled", true);
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPointListingResponse)));
    Mockito.when(categoryRepository.findHierarchyByCategoryCodes(categoryCodeRequestArgumentCaptor.capture()))
        .thenReturn(Arrays.asList(
            new CategoryHierarchyResponse(DEFAULT_CATEGORY_CODE, DEFAULT_CATEGORY_CODE, 1, categoriesData)));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequestArgumentCaptor.capture()))
        .thenReturn(Arrays.asList(productItemResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(campaignOutbound.getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(
            productLevel3Service.getProductItemCogsValues(DEFAULT_BUSINESS_PARTNER_CODE, Arrays.asList(DEFAULT_SKU_CODE)))
        .thenReturn(ImmutableMap.of(DEFAULT_SKU_CODE, productItemsCogs));
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito.when(
            productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);
    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(),false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(campaignOutbound).getCampaignPriceInfoV2(Mockito.any(CampaignPriceRequest.class));
    Mockito.verify(
        productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsEmptyResultTest() throws Exception {
    Mockito.when(xProductOutbound.getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito
      .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(xProductOutbound)
        .getItemPickupPointList(eq(STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), eq(0),
            eq(1), Mockito.any(ItemPickupPointListingRequest.class));
  }

  @Test
  public void getItemPickupPointL3ListingInvalidBusinessPartnerCodeTest() throws Exception {
    try {
      Mockito.when(
              productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
          .thenReturn(Arrays.asList(itemImageResponse));
      Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any())).thenReturn(null);
      productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
          itemPickupPointListingL3Request, false, false, true);
    }
    catch (ApplicationRuntimeException applicationRuntimeException){
      Assertions.assertEquals("Can not process invalid input data :Invalid business partner code",
          applicationRuntimeException.getErrorMessage());
    }
    finally {
      Mockito
          .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());

    }
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsTest() throws Exception {
    productItemBusinessPartner.setFbbActive(true);
    productBusinessPartner.setProductItemBusinessPartners(Collections.singletonList(productItemBusinessPartner));
    itemPickupPointListingL3Request.setNeedCorrection(true);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList())).thenReturn(Arrays.asList(productLevel3Inventory));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Page<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponsePage =
        productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito.verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true);
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(productPricingOutbound).getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class));
    Assertions.assertTrue(itemPickupPointListingL3ResponsePage.getContent().get(0).isFbbActivated());
    Mockito
      .verify(this.businessPartnerRepository, times(2)).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsMppFalseTest() throws Exception {
    itemPickupPointListingL3Request.setNeedCorrection(true);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());

    Mockito.when(
            productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository)
        .findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerForL5Listing(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID, 0, 1,
            itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito.verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true);
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productItemWholesalePriceService, times(2))
        .findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID), Mockito.anyList());
    Mockito
      .verify(this.businessPartnerRepository, times(2)).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsMppTrueTest() throws Exception {
    itemPickupPointListingL3Request.setNeedCorrection(true);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());

    Mockito.when(
            productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(
            productPricingOutbound.getWholesalePriceListV2(Mockito.any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository)
        .findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerForL5Listing(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID, 0, 1,
            itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito.verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true);
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productItemWholesalePriceService, times(2))
        .findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID), Mockito.anyList());
    Mockito
      .verify(this.businessPartnerRepository, times(2)).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsNotActiveTest() throws Exception {
    itemPickupPointListingL3Request.setNeedCorrection(true);
    productItemResponse.getImages().forEach(image1 -> image1.setOriginalImage(true));
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setPostLive(false);
    productCollection.setEdited(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantType(Constants.CC_MERCHANT);
    Mockito.when(
            productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
   Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemBusinessPartner));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(profileResponse);
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository)
        .findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerForL5Listing(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID, 0, 1,
            itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito.verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true);
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productItemWholesalePriceService, times(2))
        .findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(mapperUtil).mapStringToResponse(Mockito.any());
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerByItemSkuList(eq(STORE_ID), Mockito.anyList());
    Mockito
      .verify(this.businessPartnerRepository, times(2)).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsNotActiveWrongItemIdTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "backFillWrongProductItemIds", true);
    itemPickupPointListingL3Request.setNeedCorrection(true);
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.name());
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productItemResponse.getImages().forEach(image1 -> image1.setOriginalImage(true));
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setPostLive(false);
    productCollection.setEdited(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantType(Constants.CC_MERCHANT);
    Mockito.when(
            productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productOutbound.getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true))
        .thenReturn(productDetailResponse);
    Mockito.when(productOutbound.getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.capture(), Mockito.eq(false)))
        .thenReturn(Arrays.asList(itemImageResponse));
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productCollection));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemWholesalePrice));
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(eq(STORE_ID),
        Mockito.anyList())).thenReturn(Arrays.asList(productItemBusinessPartner));
    Mockito
        .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(profileResponse);
    Mockito.doNothing().when(productItemBusinessPartnerService).saveAll(Mockito.anyList());

    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository)
        .findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerForL5Listing(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID, 0, 1,
            itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito.verify(productOutbound).getProductDetailByProductCode(DEFAULT_PRODUCT_CODE, false, true);
    Mockito.verify(productOutbound).getProductItemImagesByItemCode(skuCodesRequestArgumentCaptor.getValue(), false);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productItemWholesalePriceService, times(2))
        .findByStoreIdAndItemSkuAndPickupPointCode(eq(STORE_ID), Mockito.anyList());
    Mockito.verify(mapperUtil).mapStringToResponse(Mockito.any());
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemBusinessPartnerByItemSkuList(eq(STORE_ID), Mockito.anyList());
    Mockito
        .verify(this.businessPartnerRepository, times(2)).filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(productItemBusinessPartnerService).saveAll(Mockito.anyList());
  }

  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsNullProductTest() throws Exception {
    itemPickupPointListingL3Request.setNeedCorrection(true);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(Arrays.asList(productItemBusinessPartner)));
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID)))
        .thenReturn(new ArrayList<>());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemBusinessPartnerForL5Listing(STORE_ID,
        PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductIds(STORE_ID, Arrays.asList(PRODUCT_ID));
    Mockito
      .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
  }


  @Test
  public void getItemPickupPointL3ListingNeedCorrectionProductsEmptyResultTest() throws Exception {
    itemPickupPointListingL3Request.setNeedCorrection(true);
    ProductDetailResponse productDetailResponse = generateProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito
      .when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(getProfileResponse());
    productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
        itemPickupPointListingL3Request, false, false, true);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_BUSINESS_PARTNER_CODE + DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemBusinessPartnerForL5Listing(STORE_ID,
        PRODUCT_BUSINESS_PARTNER_ID, 0, 1, itemPickupPointListingL3Request);
    Mockito
      .verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
  }


  @Test
  public void getItemPickupPointL3ListingActiveProductsEmptyBpCodeTest() throws Exception {
    itemPickupPointListingL3Request.setBusinessPartnerCode(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
          itemPickupPointListingL3Request, false, false, true);
    });
  }

  @Test
  public void getItemPickupPointL3ListingActiveProductsEmptyProductSkuTest() throws Exception {
    itemPickupPointListingL3Request.setProductSku(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
          itemPickupPointListingL3Request, false, false, true);
    });
  }

  @Test
  public void getItemPickupPointL3ListingActiveDifferentProductSkuAndMerchantTest() throws Exception {
    itemPickupPointListingL3Request.setBusinessPartnerCode(PRODUCT_BUSINESS_PARTNER_ID);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean.getItemPickupPointL3Listing(STORE_ID, USERNAME, REQUEST_ID, 0, 1,
          itemPickupPointListingL3Request, false, false, true);
    });
  }

  @Test
  public void getItemPickupPointByProductSkusTest() throws Exception {
    Mockito.when(this.xProductOutbound.getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest))).thenReturn(
      new PageImpl<>(Collections.singletonList(itemResponseV2), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          Mockito.anyList()))
      .thenReturn(Collections.singletonList(productLevel3Inventory));
    Page<ProductLevel3SummaryResponse> response =
      productL3ServiceBean.getItemPickupPointByProductSkus(PAGE, SIZE,
        Collections.singletonList(DEFAULT_PRODUCT_SKU), DEFAULT_BUSINESS_PARTNER_CODE, false, null);
    Mockito.verify(this.xProductOutbound).getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest));
    Mockito.verify(productLevel3InventoryService)
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getContent()));
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(AVAILABLE_STOCK_1, response.getContent().get(0).getAvailableStockLevel1(),
      0);
    Assertions.assertEquals(AVAILABLE_STOCK_2, response.getContent().get(0).getAvailableStockLevel2(),
      0);
    Assertions.assertTrue(response.getContent().get(0).getSynchronizeStock());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, response.getContent().get(0).getItemSku());
    Assertions.assertEquals(PRICE, response.getContent().get(0).getPrices().get(0).getPrice(), 0.0);
    Assertions.assertEquals(SALE_PRICE, response.getContent().get(0).getPrices().get(0).getSalePrice(), 0.0);
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(DEFAULT_PICKUP_POINT_CODE,
      response.getContent().get(0).getPickupPointCode());
  }

  @Test
  public void getItemPickupPointByProductSkus_onlineOrCncTest() throws Exception {
    itemPickupPointSummaryRequest.setOnlineOrCnc(true);
    Mockito.when(this.xProductOutbound.getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest))).thenReturn(
      new PageImpl<>(Collections.singletonList(itemResponseV2), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          Mockito.anyList()))
      .thenReturn(Collections.singletonList(productLevel3Inventory));
    Page<ProductLevel3SummaryResponse> response =
      productL3ServiceBean.getItemPickupPointByProductSkus(PAGE, SIZE,
        Collections.singletonList(DEFAULT_PRODUCT_SKU), DEFAULT_BUSINESS_PARTNER_CODE, true, null);
    Mockito.verify(this.xProductOutbound).getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest));
    Mockito.verify(productLevel3InventoryService)
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getContent()));
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(AVAILABLE_STOCK_1, response.getContent().get(0).getAvailableStockLevel1(),
      0);
    Assertions.assertEquals(AVAILABLE_STOCK_2, response.getContent().get(0).getAvailableStockLevel2(),
      0);
    Assertions.assertTrue(response.getContent().get(0).getSynchronizeStock());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, response.getContent().get(0).getItemSku());
    Assertions.assertEquals(PRICE, response.getContent().get(0).getPrices().get(0).getPrice(), 0.0);
    Assertions.assertEquals(SALE_PRICE, response.getContent().get(0).getPrices().get(0).getSalePrice(), 0.0);
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(DEFAULT_PICKUP_POINT_CODE,
      response.getContent().get(0).getPickupPointCode());
  }

  @Test
  public void getItemPickupPointByProductSkus_emptyL5ResponseTest() throws Exception {
    Mockito.when(this.xProductOutbound.getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest))).thenReturn(
      new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE, SIZE), SIZE));
    Page<ProductLevel3SummaryResponse> response =
      productL3ServiceBean.getItemPickupPointByProductSkus(PAGE, SIZE,
        Collections.singletonList(DEFAULT_PRODUCT_SKU), DEFAULT_BUSINESS_PARTNER_CODE, false, null);
    Mockito.verify(this.xProductOutbound).getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest));
    Assertions.assertTrue(CollectionUtils.isEmpty(response.getContent()));
  }

  @Test
  public void getItemPickupPointByProductSkus_emptyRequestTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      Page<ProductLevel3SummaryResponse> response =
          productL3ServiceBean.getItemPickupPointByProductSkus(PAGE, SIZE,
              Collections.emptyList(), DEFAULT_BUSINESS_PARTNER_CODE, false, null);
    });
  }

  @Test
  public void getItemPickupPointByProductSkus_emptyInventoryResponseTest() throws Exception {
    Mockito.when(this.xProductOutbound.getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest))).thenReturn(
      new PageImpl<>(Collections.singletonList(itemResponseV2), PageRequest.of(PAGE, SIZE), SIZE));
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          Mockito.anyList()))
      .thenReturn(Collections.emptyList());
    Page<ProductLevel3SummaryResponse> response =
      productL3ServiceBean.getItemPickupPointByProductSkus(PAGE, SIZE,
        Collections.singletonList(DEFAULT_PRODUCT_SKU), DEFAULT_BUSINESS_PARTNER_CODE, false, null);
    Mockito.verify(this.xProductOutbound).getItemPickupPointSummary(eq(PAGE), eq(SIZE),
      eq(itemPickupPointSummaryRequest));
    Mockito.verify(productLevel3InventoryService)
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getContent()));
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertNull(response.getContent().get(0).getAvailableStockLevel1());
    Assertions.assertNull(response.getContent().get(0).getAvailableStockLevel2());
    Assertions.assertNull(response.getContent().get(0).getSynchronizeStock());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, response.getContent().get(0).getItemSku());
    Assertions.assertEquals(PRICE, response.getContent().get(0).getPrices().get(0).getPrice(), 0.0);
    Assertions.assertEquals(SALE_PRICE, response.getContent().get(0).getPrices().get(0).getSalePrice(), 0.0);
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(response.getContent().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(DEFAULT_PICKUP_POINT_CODE,
      response.getContent().get(0).getPickupPointCode());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteExistInXproductTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku(DEFAULT_PRODUCT_SKU).productExists(true).build()));


    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU));
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteExistInXproductBatchTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku(DEFAULT_PRODUCT_SKU).productExists(true).build()));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU));
  }


  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteExistInXproductMppSellerTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 2);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku(DEFAULT_PRODUCT_SKU).productExists(true).build()));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU));
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteExistInXproductMppSellerBatchFlowTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 2);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU))).thenReturn(
        Arrays.asList(ProductBasicResponse.builder().productSku(DEFAULT_PRODUCT_SKU).productExists(true).build()));


    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU));
  }


  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteProductBusinessPartnerNotFoundTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);

    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantType(Constants.TD_MERCHANT);

    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMppTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    InventoryDetailInfoResponseV2DTO inventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryDetailInfoResponseV2DTO), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp2Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp7Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchItemCodeFromPcb", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp3Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(null);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(InventoryDetailInfoResponseV2DTO), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp4Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(InventoryDetailInfoResponseV2DTO), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveNewFlowTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "deletePickupPointNewFlow", true);
    itemSkuPickupPointRequest.setItemSkuList(List.of(DEFAULT_ITEM_SKU));
    itemSkuPickupPointRequest.setPickupPointCode(PICKUP);
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(PICKUP);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            DEFAULT_STORE_ID, DEFAULT_ITEM_SKU, PICKUP)).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        productBusinessPartner.getProductId())).thenReturn(productCollection);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(InventoryDetailInfoResponseV2DTO), null, REQUESTID));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(productItemBusinessPartnerRepository).saveAll(productItemBusinessPartnerListCaptor.capture());
    Assertions.assertEquals(2, productItemBusinessPartnerListCaptor.getValue().size(), 0);
    Assertions.assertEquals(1,
        productItemBusinessPartnerListCaptor.getValue().stream().filter(GdnBaseEntity::isMarkForDelete).count());
    Assertions.assertEquals(PICKUP,
        productItemBusinessPartnerListCaptor.getValue().stream().filter(GdnBaseEntity::isMarkForDelete).findFirst()
            .get().getPickupPointId());
    Assertions.assertEquals(DEFAULT_PICKUP_POINT_CODE,
        productItemBusinessPartnerListCaptor.getValue().stream().filter(Predicate.not(GdnBaseEntity::isMarkForDelete))
            .findFirst().get().getPickupPointId());
    Assertions.assertEquals(1,
        productItemBusinessPartnerListCaptor.getValue().stream().filter(Predicate.not(GdnBaseEntity::isMarkForDelete))
            .count());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp5Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchL4WithOutMFDFilterForUpdatePP", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(
      productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointId(
        anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse());
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp6Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppForWhEnabled", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Map<String, String> stringStringMap = new HashMap<>();
    stringStringMap.put(PICKUP, PICKUP);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse(stringStringMap));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_SKU);
    Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), Mockito.any()))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, itemBasicDetailV2Response));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp8Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchItemCodeFromPcb", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Map<String, String> stringStringMap = new HashMap<>();
    stringStringMap.put(PICKUP, PICKUP);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse(stringStringMap));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(DEFAULT_ITEM_SKU);
    itemBasicDetailV2Response.setItemCode(ITEM_SKU);
    ItemBasicDetailV2Response itemBasicDetailV2Response1 = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response1.setItemSku(DEFAULT_ITEM_SKU_1);
    itemBasicDetailV2Response1.setItemCode(ITEM_SKU);
    Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), Mockito.any()))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, itemBasicDetailV2Response1));
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp9Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchItemCodeFromPcb", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Map<String, String> stringStringMap = new HashMap<>();
    stringStringMap.put(PICKUP, PICKUP);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse(stringStringMap));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(PICKUP);
    itemBasicDetailV2Response.setItemCode(PICKUP);
    Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), Mockito.any()))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, itemBasicDetailV2Response));
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any())).thenReturn(null);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp10Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchItemCodeFromPcb", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Map<String, String> stringStringMap = new HashMap<>();
    stringStringMap.put(PICKUP, PICKUP);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse(stringStringMap));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(PICKUP);
    itemBasicDetailV2Response.setItemCode(PICKUP);
    Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), Mockito.any()))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, itemBasicDetailV2Response));
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any())).thenReturn(simpleStringMapResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteActiveMpp11Test() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "fetchItemCodeFromPcb", true);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    productCollection.setEdited(true);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);

    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();

    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);

    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(1);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    InventoryDetailInfoResponseV2DTO InventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    InventoryDetailInfoResponseV2DTO.setWebInventoryResponse(new WebInventoryResponseV2DTO());
    Map<String, String> stringStringMap = new HashMap<>();
    stringStringMap.put(PICKUP, PICKUP);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any()))
        .thenReturn(new SimpleStringMapResponse(stringStringMap));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(PICKUP);
    itemBasicDetailV2Response.setItemCode(PICKUP);
    Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), Mockito.any()))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, itemBasicDetailV2Response));
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> map = new HashMap<>();
    map.put(productItemBusinessPartner.getProductItemId(), ITEM_NAME);
    simpleStringMapResponse.setMapResponse(map);
    Mockito.when(productRepository.getSkuCodesByProductItemIds(Mockito.any())).thenReturn(simpleStringMapResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);

    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }


  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestForFbbfalseCncFalse() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActive(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestForFbbCnc() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestForFalse() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setPostLive(true);
    productCollection.setEdited(false);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestFbbFalseCncTrue() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponse1.setFbbActivated(true);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setPostLive(true);
    productCollection.setEdited(false);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            DEFAULT_BUSINESS_PARTNER_CODE, true)).thenReturn(productItemBusinessPartner);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestPostLiveFalse() throws Exception {
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestPostLiveFalseBatchTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedFalse() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "mppAllowedSellers", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(true);
    productCollection.setPostLive(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedBatchFalse() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppAllowedSellers", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(true);
    productCollection.setPostLive(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedBatchAddAndDeleteFalse() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 2);
    ReflectionTestUtils.setField(productL3ServiceBean, "mppAllowedSellers", "TD");
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(true);
    productCollection.setPostLive(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedTrue() throws Exception {
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(false);
    productItemBusinessPartner.setCncActivated(false);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(false);
    productCollection.setPostLive(false);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedTrue_preOrder()
      throws Exception {
    try (MockedStatic<ConverterUtil> mockedCommonUtils = Mockito.mockStatic(ConverterUtil.class)) {
      mockedCommonUtils.when(() -> ConverterUtil.isMPPEnabled(any(), any())).thenReturn(true);
      Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
      ReflectionTestUtils.setField(productL3ServiceBean, "itemCodeFetchSize", 10);
      List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
      ProductBasicResponse productBasicResponse = new ProductBasicResponse();
      productBasicResponse.setProductExists(true);
      productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
      PreOrderDTO preOrder = new PreOrderDTO();
      preOrder.setPreOrderDate(new Date());
      productBasicResponse.setPreOrder(preOrder);
      productBasicResponses.add(productBasicResponse);
      itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
      List<PickupPointResponse> pickupPointResponses = new ArrayList<>();
      PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
      pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
      pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
      pickupPointResponses.add(pickupPointResponse1);
      productBusinessPartner.setState(NEED_CORRECTION);
      productBusinessPartner.setProductId(PRODUCT_ID);
      productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
      productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
      productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
      productItemBusinessPartner.setFbbActive(false);
      productItemBusinessPartner.setCncActivated(false);
      Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
      Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
      updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
      WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
          new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
      updatePickupPointResponse.setErrors(Arrays.asList(error));
      productCollection.setEdited(false);
      productCollection.setPostLive(false);
      itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
      itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
      productBusinessPartner.setProductId(PRODUCT_ID);
      productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
      GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response =
          new GdnRestSingleResponse<>();
      response.setSuccess(true);
      Mockito.when(
              xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
          .thenReturn(productBasicResponses);
      Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
          .thenReturn(productBusinessPartner);
      Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID,
          DEFAULT_ITEM_SKU)).thenReturn(2);
      Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID,
          DEFAULT_ITEM_SKU_1)).thenReturn(1);
      Mockito.when(
              businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
          .thenReturn(pickupPointResponses);
      Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
          DEFAULT_STORE_ID, PRODUCT_ID)).thenReturn(productCollection);
      Mockito.when(
          productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
              anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
      Mockito.doNothing().when(productLevel3ServiceBean)
          .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap,
              itemXNewPickUpPointMap, null);
      Mockito.when(
          productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
              productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
          CHANNEL_ID);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          REQUESTID);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
      org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
      Mockito.when(inventoryOutbound.updatePickupPoint(any(), any()))
          .thenReturn(updatePickupPointResponse);
      Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
          Mockito.anyString(), Mockito.anyString())).thenReturn(productCollection);
      
      // Add required mocks for MPP flow
      ProfileResponse profileResponse = getProfileResponse();
      profileResponse.getCompany().setCncActivated(true);
      Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
          .thenReturn(profileResponse);
      
      // Mock x-product call for fetching item basic details (needed for MPP flow)
      List<ItemBasicDetailV2Response> itemBasicDetails = new ArrayList<>();
      ItemBasicDetailV2Response itemBasicDetail = new ItemBasicDetailV2Response();
      itemBasicDetail.setItemSku(DEFAULT_ITEM_SKU);
      itemBasicDetail.setItemCode("MTA-00001-00001");
      itemBasicDetails.add(itemBasicDetail);
      Mockito.when(xProductOutbound.getItemBasicDetailsByItemSkus(eq(true), any()))
          .thenReturn(itemBasicDetails);
      
      productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID,
          itemSkuPickupPointRequest);
      Mockito.verify(productBusinessPartnerRepository)
          .findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
      Mockito.verify(productItemBusinessPartnerService)
          .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
      Mockito.verify(productItemBusinessPartnerService)
          .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
      Mockito.verify(productBusinessPartnerRepository, Mockito.times(1))
          .findFirstByGdnProductSku("BLI-00001-00001");
      Mockito.verify(productCollectionRepository)
          .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());

    }
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestEditedTrueBatchTest() throws Exception {
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeletePpCode", true);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateInsertBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(productL3ServiceBean, "inventoryBatchUpdateSize", 10);
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(false);
    productItemBusinessPartner.setCncActivated(false);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setEdited(false);
    productCollection.setPostLive(false);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(Mockito.any()))
        .thenReturn(pickupPointResponseList);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(productItemBusinessPartner);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointWithAllToDeleteTestForFalseFailed() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    pickupPointResponse1.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponseList.add(pickupPointResponse1);
    productBusinessPartner.setState(NEED_CORRECTION);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setFbbActive(true);
    productItemBusinessPartner.setCncActivated(true);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    updatePickupPointResponse = new WebInventoryUpdatePickupPointResponseDTO();
    WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError error =
        new WebInventoryUpdatePickupPointResponseDTO.UpdatePickupPointError();
    updatePickupPointResponse.setErrors(Arrays.asList(error));
    productCollection.setPostLive(true);
    productCollection.setEdited(true);
    itemXNewPickUpPointMap.put(DEFAULT_ITEM_SKU_1, DEFAULT_PICKUP_POINT_CODE);
    itemXOldPickUpPointMap.put(DEFAULT_ITEM_SKU_1, PICKUP);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU))
        .thenReturn(2);
    Mockito.when(productItemBusinessPartnerService.getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1))
        .thenReturn(1);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(pickupPointFilterRequest))
        .thenThrow(RuntimeException.class);
    Mockito.when(
            productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(DEFAULT_STORE_ID, PRODUCT_ID))
        .thenReturn(productCollection);
    Mockito.when(
        productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
            anyString(), anyString(), anyString())).thenReturn(productItemBusinessPartner);
    Mockito.doNothing().when(productLevel3ServiceBean)
        .updateWholesaleEntriesOnPickupPointChange("10001", itemXOldPickUpPointMap, itemXNewPickUpPointMap, null);
    Mockito.when(
        productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(
            productBusinessPartner.getId(), true)).thenReturn(null);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STOREID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUESTID);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    org.apache.log4j.MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, null);
    Mockito.when(inventoryOutbound.updatePickupPoint(any(), any())).thenReturn(updatePickupPointResponse);
    productL3ServiceBean.deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productItemBusinessPartnerService).getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerService)
        .getProductItemCountByItemSku(DEFAULT_STORE_ID, DEFAULT_ITEM_SKU_1);
    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1)).findFirstByGdnProductSku("BLI-00001-00001");
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }
  @Test
  public void deleteInProgressL5ForDeletePickupPointDeletedStateTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    productBusinessPartner.setState(DELETED);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean
      .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointInvalidTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Arrays.asList(DEFAULT_ITEM_SKU, DEFAULT_ITEM_SKU_1));
    productBusinessPartner.setState("INVALID");
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(pickupPointResponseList);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    productL3ServiceBean
      .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointEmptyProductSkuTest() throws Exception {
    itemSkuPickupPointRequest.setProductSku(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean
          .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    });
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointEmptyPPCodeTest() throws Exception {
    itemSkuPickupPointRequest.setPickupPointCode(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean
          .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    });
  }

  @Test
  public void deleteInProgressL5ForDeletePickupPointEmptyItemListTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(new ArrayList<>());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productL3ServiceBean
          .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    });
  }

  @Test
  public void deleteActiveL5ForTest() throws Exception {
    itemSkuPickupPointRequest.setItemSkuList(Collections.singletonList(DEFAULT_ITEM_SKU));
    productBusinessPartner.setState(ACTIVE);
    productCollection.setState(ACTIVE);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productBusinessPartner);
    Mockito.when(xProductOutbound.deleteActiveItemPickupPointByPickupPointCode(deleteItemPickupPointRequest))
      .thenReturn(responseList);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest)).thenReturn(pickupPointResponseList);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    ProfileResponse profileResponse=getProfileResponse();
    profileResponse.getCompany().setMerchantType(Constants.TD_MERCHANT);
    Mockito.when(xProductOutbound.getL5CountByItemSku(DEFAULT_ITEM_SKU)).thenReturn(new SimpleLongResponse(1L));
    productL3ServiceBean
      .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(xProductOutbound).getL5CountByItemSku(DEFAULT_ITEM_SKU);
  }

  @Test
  public void deleteActiveL5ForExistingProductTest() throws Exception {
    List<ProductBasicResponse> productBasicResponses = new ArrayList<>();
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductExists(true);
    productBasicResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    productBasicResponses.add(productBasicResponse);
    itemSkuPickupPointRequest.setItemSkuList(Collections.singletonList(DEFAULT_ITEM_SKU));
    productBusinessPartner.setState(ACTIVE);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productCollection.setState(ACTIVE);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU)))
        .thenReturn(productBasicResponses);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    Mockito.when(xProductOutbound.deleteActiveItemPickupPointByPickupPointCode(deleteItemPickupPointRequest))
        .thenReturn(responseList);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest)).thenReturn(pickupPointResponseList);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(productCollection);
    ProfileResponse profileResponse=getProfileResponse();
    profileResponse.getCompany().setMerchantType(Constants.TD_MERCHANT);
    Mockito.when(xProductOutbound.getL5CountByItemSku(DEFAULT_ITEM_SKU)).thenReturn(new SimpleLongResponse(2L));
    productL3ServiceBean
        .deleteInProgressL5ForDeletePickupPoint(DEFAULT_STORE_ID, itemSkuPickupPointRequest);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(xProductOutbound).getL5CountByItemSku(DEFAULT_ITEM_SKU);
  }

  @Test
  public void autoHealMainImageUrlTest() {
    productL3ServiceBean.autoHealMainImageUrl(
        Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse), Arrays.asList(DEFAULT_SKU_CODE),
        Map.of(DEFAULT_SKU_CODE, itemImageResponse));
  }

  @Test
  public void autoHealMainImageUrlSkuListEmptyTest() {
    productL3ServiceBean.autoHealMainImageUrl(
        Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse), new ArrayList<>(),
        Map.of(DEFAULT_SKU_CODE, itemImageResponse));
  }

  @Test
  public void autoHealMainImageUrlMainImageNonEmptyTest() {
    itemPickupPointListingResponse.setMainImageUrl(MAIN_IMAGE_URL);
    productL3ServiceBean.autoHealMainImageUrl(
        Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse), Arrays.asList(DEFAULT_SKU_CODE),
        Map.of(DEFAULT_SKU_CODE, itemImageResponse));
  }

  @Test
  public void autoHealMainImageUrlMainImageUrlSameTest() {
    itemImageResponse.getImageResponses().get(0).setMainImage(true);
    itemImageResponse.getImageResponses().get(0).setActive(true);
    itemPickupPointListingResponse.setMainImageUrl(DEFAULT_LOCATION_PATH);
    productL3ServiceBean.autoHealMainImageUrl(
        Arrays.asList(itemPickupPointListingResponse, itemPickupPointListingResponse), Arrays.asList(DEFAULT_SKU_CODE),
        Map.of(DEFAULT_SKU_CODE, itemImageResponse));
  }

  @Test
  public void getMainImageUrlFromItemImageResponseTest() {
    itemImageResponse.setImageResponses(null);
    String mainImageUrl = productL3ServiceBean.getMainImageUrlFromItemImageResponse(itemImageResponse);
    Assertions.assertEquals(StringUtils.EMPTY, mainImageUrl);
  }
  @Test
  public void getMainImageUrlFromItemImageResponseMainImageFalseTest() {
    itemImageResponse.getImageResponses().get(0).setActive(true);
    itemImageResponse.getImageResponses().get(0).setMainImage(false);
    String mainImageUrl = productL3ServiceBean.getMainImageUrlFromItemImageResponse(itemImageResponse);
    Assertions.assertEquals(StringUtils.EMPTY, mainImageUrl);
  }

  @Test
  public void getMainImageUrlFromItemImageResponseMainImageFalseContainsResizeTest() {
    itemImageResponse.getImageResponses().get(0).setActive(true);
    itemImageResponse.getImageResponses().get(0).setMainImage(true);
    itemImageResponse.getImageResponses().get(0).setLocationPath(RESIZE_IMAGE_URL);
    String mainImageUrl = productL3ServiceBean.getMainImageUrlFromItemImageResponse(itemImageResponse);
    Assertions.assertEquals(StringUtils.EMPTY, mainImageUrl);
  }

  @Test
  public void getProductSkuDetailResponseByProductSkuTest() throws Exception {
    productCenterDetailResponse.setStatus(SUSPENDED);
    productCollection.setMarkForDelete(true);
    productCodeAndSkuRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    Mockito.when(productCollectionRepository.findProductByGdnSku(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCollection);
    Mockito.when(xProductOutbound.getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCenterDetailResponse);
    ProductSkuDetailResponse productSkuDetailResponse =
      productL3ServiceBean.getProductSkuDetailResponse(STORE_ID, productCodeAndSkuRequest);
    Mockito.verify(productCollectionRepository).findProductByGdnSku(DEFAULT_PRODUCT_SKU);
    Assertions.assertTrue(productSkuDetailResponse.isRejected());
  }

  @Test
  public void getProductSkuDetailResponseByProductCodeTest()  {
    productCollection.setState(Constants.ACTIVE);
    productCollection.setPostLive(true);
    productCodeAndSkuRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    productCodeAndSkuRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(xProductOutbound.getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCenterDetailResponse);
    ProductSkuDetailResponse productSkuDetailResponse =
      productL3ServiceBean.getProductSkuDetailResponse(STORE_ID, productCodeAndSkuRequest);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU);
    Assertions.assertFalse(productSkuDetailResponse.isSuspended());
    Assertions.assertFalse(productSkuDetailResponse.isRejected());
  }

  @Test
  public void getProductSkuDetailResponseByProductCodeTest4()  {
    productCollection.setState(Constants.ACTIVE);
    productCodeAndSkuRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    productCodeAndSkuRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCenterDetailResponse.setMarkForDelete(true);
    Mockito.when(xProductOutbound.getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCenterDetailResponse);
    ProductSkuDetailResponse productSkuDetailResponse =
      productL3ServiceBean.getProductSkuDetailResponse(STORE_ID, productCodeAndSkuRequest);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU);
    Assertions.assertFalse(productSkuDetailResponse.isSuspended());
    Assertions.assertFalse(productSkuDetailResponse.isRejected());
  }

  @Test
  public void getProductSkuDetailResponseByProductCodeTest2() {
    productCollection.setState(Constants.IN_PROGRESS_STATE);
    productCodeAndSkuRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    productCodeAndSkuRequest.setProductCode(PRODUCT_CODE);
    productCollection.setMarkForDelete(false);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    Mockito.when(xProductOutbound.getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCenterDetailResponse);
    ProductSkuDetailResponse productSkuDetailResponse =
      productL3ServiceBean.getProductSkuDetailResponse(STORE_ID, productCodeAndSkuRequest);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertFalse(productSkuDetailResponse.isSuspended());
    Assertions.assertFalse(productSkuDetailResponse.isRejected());
  }

  @Test
  public void getProductSkuDetailResponseByProductCodeTest3() {
    productCollection.setState(Constants.ACTIVE);
    productCodeAndSkuRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    productCodeAndSkuRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);
    productCenterDetailResponse.setStatus(SUSPENDED);
    Mockito.when(xProductOutbound.getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU))
      .thenReturn(productCenterDetailResponse);
    ProductSkuDetailResponse productSkuDetailResponse =
      productL3ServiceBean.getProductSkuDetailResponse(STORE_ID, productCodeAndSkuRequest);
    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).getProductSkuDetailResponse(DEFAULT_PRODUCT_SKU);
    Assertions.assertTrue(productSkuDetailResponse.isSuspended());
    Assertions.assertFalse(productSkuDetailResponse.isRejected());
  }
}
