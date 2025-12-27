package com.gdn.partners.pcu.external.service.impl;

import com.blibli.oss.common.paging.Paging;
import com.blibli.oss.common.response.Response;

import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.fbb.core.constant.ProductConsignmentStatus;
import com.gdn.fbb.core.constant.SortOrder;
import com.gdn.fbb.core.web.model.request.CountConsignmentFormsByItemSkusRequest;
import com.gdn.fbb.core.web.model.response.v3.ConsignmentStatusResponse;
import com.gdn.fbb.core.web.model.response.v3.CountConsignmentFormsByItemSkuResponse;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pcu.external.client.feign.FbbFeign;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.client.feign.XCampaignFeign;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.InventoryDetailStockInfoRequestDTO;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.web.model.request.B2bFields;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointDeleteWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductLevel3PriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductVariantPriceStockAndImagesWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.WholesaleStatusV2Request;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointSummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.L2StockDetailResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoV2Response;
import com.gdn.partners.pcu.external.web.model.response.XProductL3SolrAutoHealEventModel;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummary;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.ReservedStockSummaryResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.enums.AdjustmentTypeEnum;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static com.gdn.partners.pcu.external.model.Constants.BULK_PROCESS_TYPE;
import static com.gdn.partners.pcu.external.model.Constants.USER_NAME;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ProductV2ServiceImplTest {

  private static final String BRAND = "brand";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String IMAGE_URL = "imageUrl";
  private static final String PRODUCT_NAME = "productName";
  private static final String DESCRIPTION = "description";
  private static final String PRODUCT_SKU_NEW = "productSku";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String PROMO = "promo";
  private static final String REQUEST_ID = "requestId";
  private static final String WAREHOUSE_ITEM_SKU = "warehouse item sku";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String ACTIVE_PROMO_BUNDLING = "activePromoBundlings";
  private static final String ACTIVE_PROMO_BUNDLING_1 = "activePromoBundlings1";
  private static final String ITEM_NAME = "itemName";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU_2 = "itemSku-2";
  private static final String ITEM_SKU_3 = "itemSku-3";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_TWO = "pickupPointCodeTwo";
  private static final String PICKUP_POINT_CODE_THREE = "pickupPointCodeThree";
  private static final String CHANGED_BY = "changedBy";
  private static final String REGULAR_PRODUCT_TYPE = "REGULAR";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String STORE_ID = "storeId";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CLIENT_ID = "clientId";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String PRODUCT_DETAIL_LINK = "productDetailLink";
  private static final String SORT_BY = ITEM_SKU;
  private static final String ORDER_BY = "asc";
  private static final String SUSPENSION_REASON = "suspensionReason";
  private static final double CAMPAIGN_PRICE = 1000.0;
  public static final boolean IS_WARE_HOUSE = false;
  public static final String BUSINESSPARTNER_CODE = "BUSINESSPARTNER_CODE";
  public static final String GDN_SKU = "GDN_SKU";
  private static final String REVIEW_TYPE = "prelive";
  private static final String PRODUCT_SKU_1 = "XXX-10001-00001";
  private static final String PRODUCT_SKU_2 = "XXX-10001-00002";
  private static final String PRODUCT_SKU_3 = "XXX-10001-00003";
  private static final String BUSINESS_PARTNER_CODE_1 = "XXX-10001";
  private static final String EMAIL = "email";
  private static final String PURCHASE_TERM = "purchase-term";
  private static final String BULK_PROCESS_CODE = "BP1234";
  private static final String EVENT_NAME = "eventName";
  private static final String EVENT_NAME_2 = "eventName2";

  private ProductL3SummaryResponse productL3SummaryResponse;
  private GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponseGdnRestListResponse;
  private ItemL4SummaryResponse itemL4SummaryResponse;
  private PageMetaData pageMetaData = new PageMetaData(PAGE, SIZE, SIZE);
  private PriceDTO priceDTO;
  private B2bFieldsDTO b2bFieldsDTO;
  private ProductSummaryV2WebRequest productSummaryV2WebRequest = new ProductSummaryV2WebRequest();;
  private ProductSummaryRequest productSummaryRequest;
  private ProductSystemParameterResponse l3StockSystemParameter;
  private InventoryStockInfoDTO inventoryStockInfoDTO;
  private InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO;
  private WarehouseInventoryResponseDTO warehouseInventoryResponseDTO;
  private WebInventoryResponseDTO webInventoryResponseDTO;
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO =
    new InventoryDetailInfoResponseDTO();
  private CampaignPriceSkuResponse campaignPriceSkuResponse;
  private GdnRestSingleResponse<CategoryNamesResponse> categoryNamesResponseGdnRestSingleResponse;
  private ProductSuspensionHistoryResponse productSuspensionHistoryResponse;
  private GdnRestListResponse<ProductSuspensionHistoryResponse>
    productSuspensionHistoryResponseGdnRestListResponse;
  private ProductSkuListRequest productSkuListRequest;
  private WholesaleStatusV2Request wholesaleStatusV2Request = new WholesaleStatusV2Request();
  private ItemPickupPointWebRequest itemPickupPointWebRequest = new ItemPickupPointWebRequest();
  private WholesalePriceSkuResponse wholesalePriceSkuResponse = new WholesalePriceSkuResponse();
  private ProductCountResponse productCountResponse = new ProductCountResponse(10L, 30L, null,
    null);
  private ReservedStockSummaryResponse reservedStockSummaryResponse = new ReservedStockSummaryResponse();
  private GdnRestSingleResponse<EditProductV2Response> editProductResponseGdnRestSingleResponse;
  private EditProductV2Response editProductResponse;
  private ProfileResponse profileResponse;
  private ProductEditInfoV2WebRequest productEditInfoWebRequest;
  private ProductL3UpdateRequest productLevel3Request;
  private EditProductWebResponse editProductWebResponse;
  private B2bFields b2bFields;


  private QuickEditV2WebRequest quickEditV2WebRequest = new QuickEditV2WebRequest();
  private ProductLevel3PriceWebRequest productLevel3PriceWebRequest =
    new ProductLevel3PriceWebRequest();
  private CompanyDTO company;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XInventoryFeign xInventoryFeign;

  @Mock
  private XCampaignFeign xCampaignFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private ProductPricingFeign productPricingFeign;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ProductService productService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private FbbFeign fbbFeign;

  @Mock
  private UserPicService userPicService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private ProductV2ServiceImpl productV2Service;

  private GdnRestSingleResponse<ProductLevel3CountResponse>
      productLevel3SummaryCountResponseGdnRestSingleResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    quickEditV2WebRequest.setPrices(Collections.singleton(productLevel3PriceWebRequest));
    quickEditV2WebRequest.setSynchronizeStock(true);
    quickEditV2WebRequest.setStatus(ProductLevel3Status.ONLINE.name());
    ReflectionTestUtils.setField(productV2Service, "productDetailPageUrlPrefix",
      PRODUCT_DETAIL_LINK);
    ReflectionTestUtils.setField(productV2Service, "inventoryApiBatchSize", 10);
    priceDTO = new PriceDTO();
    b2bFieldsDTO = new B2bFieldsDTO();
    b2bFieldsDTO.setBasePrice(100.0);
    b2bFieldsDTO.setManaged(true);
    priceDTO.setListOfDiscountPrices(Collections.singletonList(
      new DiscountPriceDTO(10.0, AdjustmentTypeEnum.BLIBLI, new Date(), new Date(), CLIENT_ID)));
    productSummaryRequest = ProductSummaryRequest.builder().archived(true)
      .categoryCodes(Collections.singletonList(CATEGORY_CODE)).inStock(true).keyword(PRODUCT_NAME)
      .merchantCode(BUSINESS_PARTNER_CODE)
      .pickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE))
      .promoTypes(Collections.singletonList(PROMO)).suspended(false).b2bActivated(true)
      .bundleProduct(false).b2cActivated(false).fetchSizeChartDetails(false).build();
    BeanUtils.copyProperties(productSummaryRequest, productSummaryV2WebRequest);
    itemL4SummaryResponse = ItemL4SummaryResponse.builder().activePromoBundlings(
        new HashSet<>(Arrays.asList(ACTIVE_PROMO_BUNDLING, ACTIVE_PROMO_BUNDLING_1)))
      .cncActivated(true).forceReview(true).createdDate(new Date()).generatedItemName(ITEM_NAME)
      .isLateFulfillment(true).itemCode(ITEM_CODE).itemSku(ITEM_SKU).wholesalePriceExists(true)
      .pickupPointCode(PICKUP_POINT_CODE).priceEditDisabled(true).productType(REGULAR_PRODUCT_TYPE)
      .promoBundling(true).updatedDate(new Date()).itemViewConfigs(Collections.singletonList(
        new ItemViewConfigDTO(true, true, CHANGED_BY, new ItemDiscoverableScheduleDTO(),
          new ItemBuyableScheduleDTO()))).markForDelete(false).merchantPromoDiscount(true)
      .merchantPromoDiscountActivated(true).merchantSku(MERCHANT_SKU)
      .promoTypes(Collections.singletonList(PROMO)).promoLabels(Collections.singletonList(PROMO))
      .version((long) 2).wholesalePriceActivated(true)
      .masterDataItemImages(Collections.singletonList(new MasterDataItemImageDTO(true, IMAGE_URL, 2))).storeId(STORE_ID)
        .price(new HashSet<>(Collections.singletonList(priceDTO))).b2bFieldsDTO(b2bFieldsDTO).build();
    productL3SummaryResponse =
      ProductL3SummaryResponse.builder().brand(BRAND).catalogCode(CATEGORY_CODE_2)
        .categoryCode(CATEGORY_CODE).isArchived(true).markForDelete(false).maxNormalPrice(100)
        .maxSellingPrice(30).variantCount(1).merchantCode(BUSINESS_PARTNER_CODE).minNormalPrice(10)
        .minSellingPrice(35).off2OnChannelActive(true).productCode(PRODUCT_CODE)
        .productMainImage(IMAGE_URL).productName(PRODUCT_NAME).productScore(
          com.gdn.x.product.rest.web.model.response.ProductScoreResponse.builder()
            .descriptionScore(30).eanUpcScore(10).imageScore(40).mandatoryAttributeScore(50)
            .productTitleScore(30).recommendedAttributeScore(56).remainingAttributeScore(4)
            .totalScore(56).uspScore(45).variantCreatingScore(6).videoUrlScore(7).build())
        .productSku(PRODUCT_SKU_NEW).promoLabels(Collections.singletonList(PROMO)).suspended(true)
        .itemL4SummaryResponse(itemL4SummaryResponse).build();
    productL3SummaryResponse.setCreatedDate(new Date());
    productL3SummaryResponse.setUpdatedDate(new Date());
    productL3SummaryResponse.setB2bActivated(true);
    productL3SummaryResponse.setB2cActivated(true);
    productL3SummaryResponse.setSizeChartCode(SIZE_CHART_CODE);
    productL3SummaryResponseGdnRestListResponse =
      new GdnRestListResponse<>(Collections.singletonList(productL3SummaryResponse), pageMetaData, REQUEST_ID);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
      .thenReturn(productL3SummaryResponseGdnRestListResponse);
    l3StockSystemParameter =
      ProductSystemParameterResponse.builder().variable(Constants.SHOW_L3_STOCK)
        .value(Boolean.TRUE.toString()).build();
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));

    inventoryDetailStockInfoRequestDTO =
      new InventoryDetailStockInfoRequestDTO(BUSINESS_PARTNER_CODE,
        Collections.singletonList(PRODUCT_SKU_NEW), Collections.singletonList(PICKUP_POINT_CODE));
    inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setWarehouseTotalAvailableStock(10);
    inventoryStockInfoDTO.setWarehouseTotalOriginalStock(40);
    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU_NEW);
    inventoryStockInfoDTO.setTotalStock(10);
    when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(false));
    when(xInventoryFeign.findDetailByWebProductSkus(!Boolean.parseBoolean(mandatoryParameterHelper.isExternalOnly()), inventoryDetailStockInfoRequestDTO)).thenReturn(
      new GdnRestListResponse<>(Collections.singletonList(inventoryStockInfoDTO), pageMetaData,
        REQUEST_ID));

    warehouseInventoryResponseDTO = new WarehouseInventoryResponseDTO();
    warehouseInventoryResponseDTO.setOriginalStock(10);
    warehouseInventoryResponseDTO.setAvailableStock(20);

    webInventoryResponseDTO = new WebInventoryResponseDTO();
    webInventoryResponseDTO.setWebItemSku(ITEM_SKU);
    webInventoryResponseDTO.setPickupPointCode(PICKUP_POINT_CODE);
    webInventoryResponseDTO.setAvailableStock(10);
    webInventoryResponseDTO.setOriginalStock(12);
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(webInventoryResponseDTO);
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(
      Collections.singletonList(warehouseInventoryResponseDTO));

    when(this.xInventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(any())).thenReturn(
      new GdnRestListResponse<>(Collections.singletonList(inventoryDetailInfoResponseDTO),
        pageMetaData, REQUEST_ID));
    campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    campaignPriceSkuResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    when(xCampaignFeign.getCampaignPriceInfo(any())).thenReturn(
      new GdnRestSingleResponse<>(CampaignPriceResponse.builder()
        .itemInfoToPriceResponse(Collections.singletonList(campaignPriceSkuResponse)).build(),
        REQUEST_ID));

    categoryNamesResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(
      new CategoryNamesResponse(Collections.singletonMap(CATEGORY_CODE, CATEGORY_NAME_1)),
      REQUEST_ID);
    when(pcbFeign.getCategoryNames(any(), anyInt(), anyInt())).thenReturn(categoryNamesResponseGdnRestSingleResponse);

    productSuspensionHistoryResponse =
      ProductSuspensionHistoryResponse.builder().productSku(PRODUCT_SKU_NEW).reason(SUSPENSION_REASON).build();
    productSuspensionHistoryResponseGdnRestListResponse =
      new GdnRestListResponse<>(Collections.singletonList(productSuspensionHistoryResponse), pageMetaData, REQUEST_ID);
    productSkuListRequest =
      new ProductSkuListRequest(Collections.singletonList(PRODUCT_SKU_NEW));
    when(pbpFeign.fetchProductSuspensionHistory(productSkuListRequest))
      .thenReturn(productSuspensionHistoryResponseGdnRestListResponse);
    when(xCampaignFeign.getCampaignPriceInfoV2(any(CampaignPriceRequest.class))).thenReturn(
      new GdnRestSingleResponse<>(CampaignPriceResponse.builder()
        .itemInfoToPriceResponse(Collections.singletonList(campaignPriceSkuResponse)).build(),
        REQUEST_ID));
    ProductLevel3CountResponse productLevel3SummaryCountResponse = new ProductLevel3CountResponse();
    Map<ProductLevel3SummaryCriteria, Long> stockConditionCounts = new HashMap<>();
    stockConditionCounts.put(ProductLevel3SummaryCriteria.REJECTED, 10L);
    stockConditionCounts.put(ProductLevel3SummaryCriteria.IN_PROGRESS, 20L);
    stockConditionCounts.put(ProductLevel3SummaryCriteria.NEED_CORRECTION, 30L);
    productLevel3SummaryCountResponse.setTotalItemsByCriterias(stockConditionCounts);
    productLevel3SummaryCountResponseGdnRestSingleResponse =
        new GdnRestSingleResponse(productLevel3SummaryCountResponse, Constants.REQUEST_ID);
    itemPickupPointWebRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointWebRequest.setItemSku(ITEM_SKU);
    wholesaleStatusV2Request.setItemPickupPointWebRequestList(
      Collections.singletonList(itemPickupPointWebRequest));
    wholesalePriceSkuResponse.setItemSku(ITEM_SKU);
    wholesalePriceSkuResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    wholesalePriceSkuResponse.setPromoActive(true);

    ReservedStockSummary reservedStockSummary = new ReservedStockSummary();
    reservedStockSummary.setReservedQuantity(10);
    ReservedStockSummary reservedStockSummary1 = new ReservedStockSummary();
    reservedStockSummary1.setReservedQuantity(7);
    reservedStockSummaryResponse.setPendingCartReserved(Arrays.asList(reservedStockSummary, reservedStockSummary1));
    reservedStockSummaryResponse.setPendingPaymentReserved(Collections.singletonList(reservedStockSummary));

    company = new CompanyDTO();
    company.setBusinessPartnerName(BUSINESSPARTNER_CODE);
    company.setMerchantFlag(true);
    company.setInternationalFlag(false);
    company.setMerchantType("CM");
    company.setOfflineToOnlineFlag(true);
    company.setInventoryFulfillment("BL");
    company.setOfflineToOnlineFlag(true);
    company.setEmail(EMAIL);
    company.setPurchaseTerm(PURCHASE_TERM);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    profileResponse.setMerchantStatus("ACTIVE");
    profileResponse.setCompany(company);

    editProductResponse = new EditProductV2Response();
    editProductResponse.setProductReview(true);
    editProductResponse.setReviewType(REVIEW_TYPE);

    productEditInfoWebRequest = new ProductEditInfoV2WebRequest();
    productEditInfoWebRequest.setProductCode(PRODUCT_CODE);
    productEditInfoWebRequest.setProductSku(PRODUCT_SKU);
    productEditInfoWebRequest.setBrand(BRAND);
    productEditInfoWebRequest.setCategoryCode(CATEGORY_CODE);
    productEditInfoWebRequest.setBusinessPartnerCode(MERCHANT_CODE);
    productEditInfoWebRequest.setDescription(DESCRIPTION);

    productLevel3Request = new ProductL3UpdateRequest();
    productLevel3Request.setProductName(PRODUCT_NAME);
    productLevel3Request.setBrand(BRAND);
    productLevel3Request.setCategoryName(CATEGORY_NAME_1);
    productLevel3Request.setDescription(DESCRIPTION);
    productLevel3Request.setBusinessPartnerCode(BUSINESSPARTNER_CODE);

    editProductWebResponse = new EditProductWebResponse();
    editProductWebResponse.setProductReview(true);
    editProductWebResponse.setReviewType(REVIEW_TYPE);

    editProductResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(editProductResponse, REQUEST_ID);
    b2bFields = new B2bFields();
    b2bFields.setManaged(true);
    b2bFields.setBasePrice(100.0);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(xProductFeign);
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(xInventoryFeign);
    Mockito.verifyNoMoreInteractions(xCampaignFeign);
    Mockito.verifyNoMoreInteractions(productPricingFeign);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(fbbFeign);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void updateItemListingTest() {
    Mockito.when(pbpFeign.itemListingUpdateV2(eq(PRODUCT_SKU),
        any(ProductLevel3QuickEditV2Request.class),eq(false)))
      .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    productV2Service.updateItemListing(PRODUCT_SKU,
      Collections.singletonList(quickEditV2WebRequest), false);
    Mockito.verify(pbpFeign)
      .itemListingUpdateV2(eq(PRODUCT_SKU), any(ProductLevel3QuickEditV2Request.class),eq(false));
  }

  @Test
  public void getProductL3ListTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(any());
    verify(this.xCampaignFeign).getCampaignPriceInfoV2(any());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
    /*Assertions.assertEquals(10,
      response.getContent().get(0).getItemPickupPointSummary().getAvailableStockLevel2(), 0);
    Assertions.assertEquals(2,
      response.getContent().get(0).getItemPickupPointSummary().getReservedStockLevel2(), 0);*/
  }

  @Test
  public void getProductL3ListRanchEnabledTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "ranchIntegrationEnabled", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setPureExternalUser(true);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Mockito.when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(true));
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(any());
    verify(this.xCampaignFeign).getCampaignPriceInfoV2(any());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    Mockito.verify(mandatoryParameterHelper,times(2)).isExternalOnly();
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
  }

  @Test
  public void getProductL3ListRanchNotEnabledTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "ranchIntegrationEnabled", false);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(any());
    verify(this.xCampaignFeign).getCampaignPriceInfoV2(any());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
  }

  @Test
  public void autoHealOosProductsTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "autoHealOosProductsWithStock", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(any());
    verify(this.xCampaignFeign).getCampaignPriceInfoV2(any());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
  }

  @Test
  public void autoHealOosProductInStockFalseTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "autoHealOosProductsWithStock", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    inventoryStockInfoDTO.setTotalStock(10);
    Mockito.when(mandatoryParameterHelper.isExternalOnly()).thenReturn(String.valueOf(true));
    when(xInventoryFeign.findDetailByWebProductSkus(false, inventoryDetailStockInfoRequestDTO)).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryStockInfoDTO), pageMetaData,
            REQUEST_ID));
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(2);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setInStock(false);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getXProductAutoHealEvent()).thenReturn(EVENT_NAME);
    Mockito.when(kafkaTopicProperties.getPbpAutoFixHistory()).thenReturn(EVENT_NAME_2);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(),any());
    verify(kafkaTopicProperties).getXProductAutoHealEvent();
    verify(mandatoryParameterHelper).getStoreId();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_SKU, new XProductL3SolrAutoHealEventModel(PRODUCT_SKU, STORE_ID, false));
    verify(kafkaTopicProperties).getPbpAutoFixHistory();
    verify(kafkaProducer).send(EVENT_NAME_2, PRODUCT_SKU, new ProductDataAutoFixHistoryListRequest(
        Collections.singletonList(
            new ProductDataAutoFixHistoryDto(PRODUCT_SKU, Constants.OOS_AUTO_HEAL, StringUtils.EMPTY))));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
  }

  @Test
  public void autoHealOosProductInStockFalseExceptionTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "autoHealOosProductsWithStock", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    inventoryStockInfoDTO.setTotalStock(10);
    when(xInventoryFeign.findDetailByWebProductSkus( false, inventoryDetailStockInfoRequestDTO)).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryStockInfoDTO), pageMetaData,
            REQUEST_ID));
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(2);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setInStock(false);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    doThrow(ApplicationRuntimeException.class).when(kafkaTopicProperties).getXProductAutoHealEvent();
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    verify(kafkaTopicProperties).getXProductAutoHealEvent();
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(ITEM_SKU,
        response.getContent().get(0).getItemPickupPointSummary().getItemSku());
    Assertions.assertEquals(ITEM_CODE,
        response.getContent().get(0).getItemPickupPointSummary().getItemCode());
    Assertions.assertEquals(ITEM_NAME,
        response.getContent().get(0).getItemPickupPointSummary().getItemName());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getWholesalePriceActivated());
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING));
    Assertions.assertTrue(
        response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings()
            .contains(ACTIVE_PROMO_BUNDLING_1));
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
    Assertions.assertNotNull(response.getContent().get(0).getItemPickupPointSummary().getB2bFields());
    Assertions.assertTrue(response.getContent().get(0).isCncActivated());
  }

  @Test
  public void autoHealOosProductInStockFalseNoL5Test() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    ReflectionTestUtils.setField(productV2Service, "autoHealOosProductsWithStock", true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setCncActivated(false);
    productL3SummaryResponse.getItemL4SummaryResponse().setCncActivated(true);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryRequest.setFetchSizeChartDetails(true);
    inventoryStockInfoDTO.setTotalStock(10);
    when(xInventoryFeign.findDetailByWebProductSkus( false, inventoryDetailStockInfoRequestDTO)).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(inventoryStockInfoDTO), pageMetaData,
            REQUEST_ID));
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setL5Count(2);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setInStock(false);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setItemL4SummaryResponse(null);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    Mockito.when(pcbFeign.getBasicSizeChartDetails(List.of(SIZE_CHART_CODE))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new BasicSizeChartDetailMapResponse(
            Map.of(SIZE_CHART_CODE,
                new BasicSizeChartDetailResponse(SIZE_CHART_CODE, SIZE_CHART_NAME))), REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getXProductAutoHealEvent()).thenReturn(EVENT_NAME);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(kafkaTopicProperties.getPbpAutoFixHistory()).thenReturn(EVENT_NAME_2);
    Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(kafkaTopicProperties).getPbpAutoFixHistory();
    verify(kafkaProducer).send(EVENT_NAME_2, PRODUCT_SKU, new ProductDataAutoFixHistoryListRequest(
        Collections.singletonList(
            new ProductDataAutoFixHistoryDto(PRODUCT_SKU, Constants.OOS_AUTO_HEAL, StringUtils.EMPTY))));
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(pcbFeign).getBasicSizeChartDetails(List.of(SIZE_CHART_CODE));
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    verify(kafkaTopicProperties).getXProductAutoHealEvent();
    verify(mandatoryParameterHelper).getStoreId();
    verify(kafkaProducer).send(EVENT_NAME, PRODUCT_SKU, new XProductL3SolrAutoHealEventModel(PRODUCT_SKU, STORE_ID, false));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertTrue(response.getContent().get(0).getB2bActivated());
    Assertions.assertTrue(response.getContent().get(0).getB2cActivated());
  }

  @Test
  public void getProductL3ListTest_emptySummaryResponse() {
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
      .thenReturn(new GdnRestListResponse<>(Collections.emptyList(), pageMetaData, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    Assertions.assertTrue(CollectionUtils.isEmpty(response.getContent()));
  }

  @Test
  public void getProductL3ListValidationExceptionTest() {
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest)).thenReturn(
        new GdnRestListResponse<>(null, ErrorCategory.VALIDATION.name(), false, null));
    try {
      Assertions.assertThrows(ValidationException.class,
          () -> productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false));
    } finally {
      verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    }
  }

  @Test
  public void getProductL3List_l3ShowStockOffTest() {
    ReflectionTestUtils.setField(productV2Service, "validateResponseForProductL3Listing", true);
    l3StockSystemParameter.setValue(Boolean.FALSE.toString());
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
  }

  @Test
  public void getProductL3List_campaignLockPrice() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    campaignPriceSkuResponse.setLockPriceUpdate(true);
    itemL4SummaryResponse.setB2bFieldsDTO(null);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertTrue(
      response.getContent().get(0).getItemPickupPointSummary().isPriceEditDisabled());
    Assertions.assertEquals(10, response.getContent().get(0).getTotalStock());
    Assertions.assertEquals(10, response.getContent().get(0).getAvailableStockLevel2());
  }

  @Test
  public void getProductL3List_itemInfoToPriceResponseNull() {
    when(xCampaignFeign.getCampaignPriceInfo(any())).thenReturn(
      new GdnRestSingleResponse<>(
        CampaignPriceResponse.builder().itemInfoToPriceResponse(null).build(), REQUEST_ID));
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
  }

  @Test
  public void getProductL3List_liveCampaign() {
    campaignPriceSkuResponse.setLive(true);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertTrue(
      response.getContent().get(0).getItemPickupPointSummary().isPriceEditDisabled());
  }

  @Test
  public void getProductL3List_withCampaignPrice() {
    campaignPriceSkuResponse.setCampaignPrice(CAMPAIGN_PRICE);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(),any());
    Assertions.assertEquals(CAMPAIGN_PRICE,
      response.getContent().get(0).getItemPickupPointSummary().getCampaignPrice(), 1000.0);
  }

  @Test
  public void getProductL3List_nullItemSummaryTest() {
    productL3SummaryResponse.setL5Count(1);
    productL3SummaryResponse.setItemL4SummaryResponse(null);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    Assertions.assertNull(response.getContent().get(0).getItemPickupPointSummary());
  }

  @Test
  public void getProductL3List_nullWarehouseInventoryTest() {
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(Collections.emptyList());
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertEquals(0,
      response.getContent().get(0).getItemPickupPointSummary().getAvailableStockLevel1());
    Assertions.assertEquals(0,
      response.getContent().get(0).getItemPickupPointSummary().getReservedStockLevel1());
  }

  @Test
  public void getProductL3List_nullActivePromoBundlineTest() {
    productL3SummaryResponse.getItemL4SummaryResponse().setActivePromoBundlings(null);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(),any());
    Assertions.assertNull(
      response.getContent().get(0).getItemPickupPointSummary().getActivePromoBundlings());
  }

  @Test
  public void getProductL3List_nullScoreResponse() {
    productL3SummaryResponse.setProductScore(null);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertNull(response.getContent().get(0).getProductScore());
  }

  @Test
  public void getProductL3List_l3ShowStockOff_nullInventoryDetailsTest() {
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(null);
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(null);
    l3StockSystemParameter.setValue(Boolean.FALSE.toString());
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
  }

  @Test
  public void getProductL3List_suspendedListTest() {
    productSummaryV2WebRequest.setSuspended(true);
    productSummaryRequest.setSuspended(true);
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(pbpFeign).fetchProductSuspensionHistory(productSkuListRequest);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
    Assertions.assertEquals(SUSPENSION_REASON, response.getContent().get(0).getSuspensionReason());
  }

  @Test
  public void getProductL3List_singleVariantProductTest() {
    productL3SummaryResponse.setVariantCount(2);
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_SKU_NEW, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME_1, response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(PROMO, response.getContent().get(0).getPromoLabels().get(0));
    Assertions.assertEquals(10, response.getContent().get(0).getMinNormalPrice());
    Assertions.assertEquals(35, response.getContent().get(0).getMinSellingPrice());
  }

  @Test
  public void getL3CountsByTypeTest() {
    ProductCountResponse productCountResponse = new ProductCountResponse(10L, 30L, null, null);
    Mockito.when(xProductFeign.getSecondaryProductCountByType(Constants.ACTIVE_STATUS, Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productCountResponse, Constants.REQUEST_ID));
    ProductL3CountWebResponse counts =
        productV2Service.getL3CountsByType(Constants.BUSINESS_PARTNER_CODE, Constants.ACTIVE_STATUS);
    Mockito.verify(xProductFeign).getSecondaryProductCountByType(Constants.ACTIVE_STATUS, Constants.BUSINESS_PARTNER_CODE);
    Assertions.assertNotNull(counts);
    Assertions.assertEquals( counts.getActive(), 10L, 0);
  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsTest() {
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "111111");

    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(1);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.singletonList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build()));
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any());

  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsForMultipleL4sTest() {
    ReflectionTestUtils.setField(productV2Service, "sizeChartAdditionForProduct", true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setSizeChartCode(null);
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest))
        .thenReturn(productL3SummaryResponseGdnRestListResponse);
    productSummaryV2WebRequest.setAttributeCode(ATTRIBUTE_CODE);
    productSummaryV2WebRequest.setSizeChartCode(SIZE_CHART_CODE);
    productSummaryV2WebRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setFetchSizeChartDetails(true);
    productSummaryRequest.setSizeAttributeCode(ATTRIBUTE_CODE);
    productSummaryRequest.setSizeChartCode(SIZE_CHART_CODE);
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO1 =
      new InventoryDetailStockInfoRequestDTO();
    inventoryDetailStockInfoRequestDTO1.setWebProductSkuList(
      Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2, PRODUCT_SKU_1));
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "111111");
    ProductL3SummaryResponse productL3SummaryResponse1 = new ProductL3SummaryResponse();
    productL3SummaryResponse1.setProductSku(PRODUCT_SKU_2);
    ItemL4SummaryResponse itemL4SummaryResponse1 = new ItemL4SummaryResponse();
    itemL4SummaryResponse1.setItemSku(ITEM_SKU_2);
    itemL4SummaryResponse1.setPrice(itemL4SummaryResponse.getPrice());
    itemL4SummaryResponse1.setItemViewConfigs(itemL4SummaryResponse.getItemViewConfigs());
    productL3SummaryResponse1.setFbbActivated(true);
    productL3SummaryResponse1.setVariantCount(1);
    productL3SummaryResponse1.setItemL4SummaryResponse(itemL4SummaryResponse1);
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    productL3SummaryResponse.setFbbActivated(true);
    productL3SummaryResponse.setVariantCount(1);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    productL3SummaryResponse.setProductSku(PRODUCT_SKU_1);
    productL3SummaryResponse.getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    inventoryDetailStockInfoRequestDTO1.setWebMerchantCode(BUSINESSPARTNER_CODE);
    List<ProductL3SummaryResponse> l3SummaryResponses =
      Arrays.asList(productL3SummaryResponse1, productL3SummaryResponse);
    GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponseGdnRestListResponse =
      new GdnRestListResponse<>();
    productL3SummaryResponseGdnRestListResponse.setContent(l3SummaryResponses);
    productL3SummaryResponseGdnRestListResponse.setSuccess(true);
    consignmentFormCountResponse.setErrors(null);
    CountConsignmentFormsByItemSkusRequest consignmentRequest =
      CountConsignmentFormsByItemSkusRequest.builder().itemSkus(Arrays.asList(ITEM_SKU_2,
          ITEM_SKU)).build();
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU_2).total(101).build()));
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(
        any(), any()))
      .thenReturn(consignmentFormCountResponse);
    when(xInventoryFeign.findDetailByWebProductSkus(anyBoolean(),any())).thenReturn(
      new GdnRestListResponse<>(Collections.singletonList(inventoryStockInfoDTO), pageMetaData,
        REQUEST_ID));
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    when(xProductFeign.getFilterSummaryL3(PAGE, SIZE,false, productSummaryRequest)).thenReturn(
      productL3SummaryResponseGdnRestListResponse);
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false,productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(),any());
    verify(fbbFeign, times(1)).countInProgressConsignmentFormsByItemSkus(
      consignmentRequest, "111111");
    Assertions.assertNotNull(response);
    Assertions.assertEquals(Optional.of(101).get(),
      response.getContent().get(0).getItemPickupPointSummary().getInProgressConsignmentCount());
    Assertions.assertEquals(Optional.of(10).get(),
      response.getContent().get(1).getItemPickupPointSummary().getInProgressConsignmentCount());
  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsTestForMergeFunction() {
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(1);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(5).build()
    ));
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class), any());
    Assertions.assertTrue(
      response.getContent().get(0).getItemPickupPointSummary().isPriceEditDisabled());
  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsTestForMergeFunctionSwitchOff() {
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", false);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(1);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(5).build()
    ));
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
    Assertions.assertTrue(
      response.getContent().get(0).getItemPickupPointSummary().isPriceEditDisabled());
  }


  @Test
  public void getProductL3List_For_inProgressConsignmentsTestForMultiVariant() {
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(2);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Arrays.asList(
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(10).build(),
      CountConsignmentFormsByItemSkuResponse.builder().itemSku(ITEM_SKU).total(5).build()
    ));
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    Page<ProductLevel3ListingV2WebResponse> response =
      productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
    verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
    verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(),any());
  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsNullTest() {
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "11111");
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(1);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(null);
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
      Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
      verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
      verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
      verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
      verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
      verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class), any());
    Assertions.assertNotNull(response);
    Assertions.assertFalse(response.getContent().stream()
      .map(ProductLevel3ListingV2WebResponse::getItemPickupPointSummary)
      .map(ItemPickupPointSummaryWebResponse::getInProgressConsignmentCount)
      .anyMatch(Objects::nonNull));
  }

  @Test
  public void getProductL3List_For_inProgressConsignmentsEmptyTest() {
    Response<List<CountConsignmentFormsByItemSkuResponse>> consignmentFormCountResponse =
      new Response<>();
    ReflectionTestUtils.setField(productV2Service, "headerAuthenticatorFbb", "11111");
    ReflectionTestUtils.setField(productV2Service, "cfCountAtListingEnabled", true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setFbbActivated(true);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).getItemL4SummaryResponse().setItemSku(ITEM_SKU);
    productL3SummaryResponseGdnRestListResponse.getContent().get(0).setVariantCount(1);
    consignmentFormCountResponse.setErrors(null);
    consignmentFormCountResponse.setData(Collections.emptyList());
    Mockito.when(fbbFeign.countInProgressConsignmentFormsByItemSkus(any(
      CountConsignmentFormsByItemSkusRequest.class),any())).thenReturn(consignmentFormCountResponse);
    Mockito.when(this.pbpFeign.findSystemParameter(Constants.SHOW_L3_STOCK))
      .thenReturn(new GdnRestSingleResponse<>(l3StockSystemParameter, REQUEST_ID));
    try {
      Page<ProductLevel3ListingV2WebResponse> response =
        productV2Service.getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    }
    finally {
      verify(xProductFeign).getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
      verify(this.pbpFeign).findSystemParameter(Constants.SHOW_L3_STOCK);
      verify(this.pcbFeign).getCategoryNames(any(), anyInt(), anyInt());
      verify(this.xInventoryFeign).findDetailByWebProductSkus(anyBoolean(), any());
      verify(fbbFeign).countInProgressConsignmentFormsByItemSkus(any(
        CountConsignmentFormsByItemSkusRequest.class),any());
    }
  }


  @Test
  public void getL3CountsByInactiveTypeTest() {
    ProductCountResponse productCountResponse = new ProductCountResponse(10L, 30L, null, null);
    Mockito.when(xProductFeign.getSecondaryProductCountByType(Constants.INACTIVE_STATUS, Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productCountResponse, Constants.REQUEST_ID));
    Mockito.when(pbpFeign.getNonActiveProductCount(Constants.BUSINESS_PARTNER_CODE, Constants.SECONDARY))
        .thenReturn(productLevel3SummaryCountResponseGdnRestSingleResponse);
    ProductL3CountWebResponse counts =
        productV2Service.getL3CountsByType(Constants.BUSINESS_PARTNER_CODE, Constants.INACTIVE_STATUS);
    Mockito.verify(xProductFeign).getSecondaryProductCountByType(Constants.INACTIVE_STATUS, Constants.BUSINESS_PARTNER_CODE);
    Mockito.verify(pbpFeign).getNonActiveProductCount(Constants.BUSINESS_PARTNER_CODE, Constants.SECONDARY);
    Assertions.assertNotNull(counts);
    Assertions.assertEquals( counts.getRejected(), 10L, 0);
  }

  @Test
  void getL3CountsByInvalidTypeTest() {
    Mockito.when(xProductFeign.getSecondaryProductCountByType(Constants.INACTIVE_STATUS,
            Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productCountResponse, Constants.REQUEST_ID));
    Mockito.when(
            pbpFeign.getNonActiveProductCount(Constants.BUSINESS_PARTNER_CODE, Constants.SECONDARY))
        .thenReturn(productLevel3SummaryCountResponseGdnRestSingleResponse);
    Assertions.assertThrows(Exception.class,
        () -> productV2Service.getL3CountsByType(Constants.BUSINESS_PARTNER_CODE,
            Constants.STORE_CODE));
  }

  @Test
  public void getL3PrimaryCountsByMerchantCodeTest() {
    Mockito.when(pbpFeign.getNonActiveProductCount(BUSINESS_PARTNER_CODE, Constants.PRIMARY))
      .thenReturn(productLevel3SummaryCountResponseGdnRestSingleResponse);
    Mockito.when(
        xProductFeign.getSecondaryProductCountByType(Constants.ACTIVE_STATUS, BUSINESS_PARTNER_CODE))
      .thenReturn(new GdnRestSingleResponse<>(productCountResponse, REQUEST_ID));
    ProductL3CountWebResponse response =
      productV2Service.getL3PrimaryCountsByMerchantCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductFeign)
      .getSecondaryProductCountByType(Constants.ACTIVE_STATUS, BUSINESS_PARTNER_CODE);
    Mockito.verify(pbpFeign).getNonActiveProductCount(BUSINESS_PARTNER_CODE, Constants.PRIMARY);
    Assertions.assertEquals(40L, response.getAll(), 0);
    Assertions.assertEquals(20L, response.getInReview(), 0);
    Assertions.assertEquals(30L, response.getNeedCorrection(), 0);
  }

  @Test
  public void getWholesaleStatusByRequestTest() {
    Mockito.when(this.productPricingFeign.getWholesalePriceSkuDetail(eq(STORE_ID), eq(REQUEST_ID),
      any(WholesalePriceSkuDetailListRequest.class))).thenReturn(
      new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(wholesalePriceSkuResponse), null, REQUEST_ID));
    List<WholesalePromoV2Response> wholesaleStatusByRequest =
      productV2Service.getWholesaleStatusByRequest(STORE_ID, REQUEST_ID, wholesaleStatusV2Request);
    Mockito.verify(this.productPricingFeign).getWholesalePriceSkuDetail(eq(STORE_ID),
      eq(REQUEST_ID), any(WholesalePriceSkuDetailListRequest.class));
    Assertions.assertEquals(ITEM_SKU, wholesaleStatusByRequest.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, wholesaleStatusByRequest.get(0).getPickupPointCode());
    Assertions.assertTrue(wholesaleStatusByRequest.get(0).isWholesalePromo());
  }

  @Test
  public void getWholesaleStatusByRequestMppOnTest() {
    ReflectionTestUtils.setField(productV2Service, "multiPickupPointEnabled", true);
    Mockito.when(this.productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID),
        any(WholesalePriceSkuDetailListRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true,
            Collections.singletonList(wholesalePriceSkuResponse), null, REQUEST_ID));
    List<WholesalePromoV2Response> wholesaleStatusByRequest =
        productV2Service.getWholesaleStatusByRequest(STORE_ID, REQUEST_ID, wholesaleStatusV2Request);
    Mockito.verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID),
        eq(REQUEST_ID), any(WholesalePriceSkuDetailListRequest.class));
    Assertions.assertEquals(ITEM_SKU, wholesaleStatusByRequest.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, wholesaleStatusByRequest.get(0).getPickupPointCode());
    Assertions.assertTrue(wholesaleStatusByRequest.get(0).isWholesalePromo());
  }

  @Test
  public void getWholesaleStatusByRequestPricingMppOnTest() {
    ReflectionTestUtils.setField(productV2Service, "pricingMultiPickupPointEnabled", true);
    Mockito.when(this.productPricingFeign.getWholesalePriceSkuDetailV2(eq(STORE_ID), eq(REQUEST_ID),
        any(WholesalePriceSkuDetailListRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true,
            Collections.singletonList(wholesalePriceSkuResponse), null, REQUEST_ID));
    List<WholesalePromoV2Response> wholesaleStatusByRequest =
        productV2Service.getWholesaleStatusByRequest(STORE_ID, REQUEST_ID, wholesaleStatusV2Request);
    Mockito.verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(eq(STORE_ID),
        eq(REQUEST_ID), any(WholesalePriceSkuDetailListRequest.class));
    Assertions.assertEquals(ITEM_SKU, wholesaleStatusByRequest.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, wholesaleStatusByRequest.get(0).getPickupPointCode());
    Assertions.assertTrue(wholesaleStatusByRequest.get(0).isWholesalePromo());
  }

  @Test
  public void getInventorySummaryTest() throws Exception {
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productV2Service.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE, PICKUP_POINT_CODE);
    Mockito.verify(xInventoryFeign).reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
  }

  @Test
  public void getInventorySummaryTest_EmptySummary() throws Exception {
    when(this.xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(), pageMetaData, REQUEST_ID));
    this.productV2Service.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE, PICKUP_POINT_CODE);
    Mockito.verify(xInventoryFeign).reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
  }

  @Test
  public void getInventorySummaryTest_webStock() throws Exception {
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, !IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productV2Service.getInventorySummary(GDN_SKU, !IS_WARE_HOUSE, BUSINESSPARTNER_CODE, PICKUP_POINT_CODE);
    Mockito.verify(xInventoryFeign).reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, !IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
  }

  @Test
  public void getInventorySummaryTest_webStockEmpty() throws Exception {
    inventoryDetailInfoResponseDTO.setWebInventoryResponse(null);
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, !IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(reservedStockSummaryResponse, REQUEST_ID));
    this.productV2Service.getInventorySummary(GDN_SKU, !IS_WARE_HOUSE, BUSINESSPARTNER_CODE, PICKUP_POINT_CODE);
    Mockito.verify(xInventoryFeign).reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, !IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
  }

  @Test
  public void getInventorySummary_EmptyResponse() throws Exception {
    inventoryDetailInfoResponseDTO.setWarehouseInventoryResponseList(null);
    when(xInventoryFeign
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any()))))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(inventoryDetailInfoResponseDTO), pageMetaData, REQUEST_ID));
    when(this.xInventoryFeign.reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE))
        .thenReturn(new GdnRestSingleResponse<>(new ReservedStockSummaryResponse(), REQUEST_ID));
    this.productV2Service.getInventorySummary(GDN_SKU, IS_WARE_HOUSE, BUSINESSPARTNER_CODE, PICKUP_POINT_CODE);
    Mockito.verify(xInventoryFeign).reservedStockSummaryByWebSKUAndPickupPointCode(GDN_SKU, PICKUP_POINT_CODE, IS_WARE_HOUSE);
    verify(xInventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(Collections.singletonList(any())));
  }

  @Test
  public void editProductV2InfoTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class), any(),
        Mockito.anyBoolean())).thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_pickupPointList() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      mockedRequestHelper.when(() -> RequestHelper.validateModifiedPickupPoints(
          productVariantPriceStockAndImagesWebRequestList)).thenReturn(pickupPointSetInput);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(3)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_multi_pickupPointList() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetInputTwo = new HashSet<>();
    pickupPointSetInputTwo.add(PICKUP_POINT_CODE);
    pickupPointSetInputTwo.add(PICKUP_POINT_CODE_TWO);
    pickupPointSetInputTwo.add(PICKUP_POINT_CODE_THREE);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ItemPickupPointWebRequest> itemPickupPointWebRequestListTwo = getItemPickupPointRequestForSingleList();
    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequestTwo =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointWebRequestListTwo).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);
    productVariantPriceStockAndImagesWebRequestList.add(productVariantPriceStockAndImagesWebRequestTwo);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      mockedRequestHelper.when(() -> RequestHelper.validateModifiedPickupPoints(
          productVariantPriceStockAndImagesWebRequestList)).thenReturn(pickupPointSetInputTwo);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(2)).validateUserPicPickupPoints(pickupPointSetInput);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetInputTwo);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_multi_updatedFalse_pickupPointList() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ItemPickupPointWebRequest> itemPickupPointWebRequestListTwo = getItemPickupPointRequestForSingleList();
    itemPickupPointWebRequestListTwo.get(0).setPickupPointNotUpdated(true);
    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequestTwo =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointWebRequestListTwo).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);
    productVariantPriceStockAndImagesWebRequestList.add(productVariantPriceStockAndImagesWebRequestTwo);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      mockedRequestHelper.when(() -> RequestHelper.validateModifiedPickupPoints(
          productVariantPriceStockAndImagesWebRequestList)).thenReturn(pickupPointSetInput);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(3)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_pickupPointList_with_empty_pickupPoints() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(new ArrayList<>()).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(2)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_pickupPointList_with_null_pickupPoints() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        new ProductVariantPriceStockAndImagesWebRequest();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(2)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_pickupPointList_defaultPP_Null() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(any(ProductL3UpdateRequest.class),
        any(), Mockito.anyBoolean()) )
        .thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      mockedRequestHelper.when(() -> RequestHelper.validateModifiedPickupPoints(
          productVariantPriceStockAndImagesWebRequestList)).thenReturn(pickupPointSetInput);
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE,
          false);
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), false);
      Mockito.verify(userPicService, times(3)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_mismatch_delete_pickupPointListInput() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequests = getDeletePickupPointRequest();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDeletePickupPoints(pickupPointDeleteRequests);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    doThrow(new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT)).when(userPicService)
        .validateUserPicPickupPoints(pickupPointSetInput);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.editProductV2Info(Mockito.any(ProductL3UpdateRequest.class), Mockito.any(),
        Mockito.anyBoolean())).thenReturn(editProductResponseGdnRestSingleResponse);
    try {
      try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
          MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
        mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
        mockedRequestHelper.when(
            () -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
                productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
        Assertions.assertThrows(ValidationException.class,
            () -> this.productV2Service.editProductV2Info(productEditInfoWebRequest,
                BUSINESSPARTNER_CODE, false));
      }
    } finally {
      Mockito.verify(userPicService).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_mismatch_add_pickupPointListInput() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    productEditInfoWebRequest.setAddPickupPoints(itemPickupPointRequestList);
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    doThrow(new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT)).when(userPicService)
        .validateUserPicPickupPoints(pickupPointSetInput);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.editProductV2Info(Mockito.any(ProductL3UpdateRequest.class), Mockito.any(),
        Mockito.anyBoolean())).thenReturn(editProductResponseGdnRestSingleResponse);
    try {
      try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
          MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
        mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
        mockedRequestHelper.when(
            () -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
                productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
        Assertions.assertThrows(ValidationException.class,
            () -> this.productV2Service.editProductV2Info(productEditInfoWebRequest,
                BUSINESSPARTNER_CODE, false));
      }
    } finally {
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_mismatch_productItem_pickupPointListInput() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    doThrow(new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT)).when(userPicService)
        .validateUserPicPickupPoints(pickupPointSetInput);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.editProductV2Info(Mockito.any(ProductL3UpdateRequest.class), Mockito.any(),
        Mockito.anyBoolean())).thenReturn(editProductResponseGdnRestSingleResponse);
    try {
      try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
          MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
        mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
        mockedRequestHelper.when(
            () -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
                productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
        mockedRequestHelper.when(() -> RequestHelper.validateModifiedPickupPoints(
            productVariantPriceStockAndImagesWebRequestList)).thenReturn(pickupPointSetInput);
        Assertions.assertThrows(ValidationException.class,
            () -> this.productV2Service.editProductV2Info(productEditInfoWebRequest,
                BUSINESSPARTNER_CODE, false));
      }
    } finally {
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetInput);
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  @Test
  public void editProductV2InfoTest_with_mismatch_default_pickupPointListInput() throws Exception {

    Set<String> pickupPointSetInput = new HashSet<>();
    pickupPointSetInput.add(PICKUP_POINT_CODE);
    pickupPointSetInput.add(PICKUP_POINT_CODE_TWO);

    Set<String> pickupPointSetOutput = new HashSet<>();
    pickupPointSetOutput.add(PICKUP_POINT_CODE);

    List<ItemPickupPointWebRequest> itemPickupPointRequestList = getItemPickupPointRequests();
    productEditInfoWebRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);

    ProductVariantPriceStockAndImagesWebRequest productVariantPriceStockAndImagesWebRequest =
        ProductVariantPriceStockAndImagesWebRequest.builder()
            .modifiedItemPickupPoints(itemPickupPointRequestList).build();
    List<ProductVariantPriceStockAndImagesWebRequest>
        productVariantPriceStockAndImagesWebRequestList = new ArrayList<>();
    productVariantPriceStockAndImagesWebRequestList.add(
        productVariantPriceStockAndImagesWebRequest);

    productEditInfoWebRequest.setProductItems(productVariantPriceStockAndImagesWebRequestList);
    doThrow(new ValidationException(ErrorMessages.NO_ACCESS_TO_PICKUP_POINT)).when(userPicService)
        .validateUserPicPickupPoints(pickupPointSetOutput);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(
        profileResponse);
    when(this.pbpFeign.editProductV2Info(Mockito.any(ProductL3UpdateRequest.class), Mockito.any(),
        Mockito.anyBoolean())).thenReturn(editProductResponseGdnRestSingleResponse);
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
      Assertions.assertThrows(ValidationException.class,
          () -> this.productV2Service.editProductV2Info(productEditInfoWebRequest,
              BUSINESSPARTNER_CODE, false));

    } finally {
      Mockito.verify(userPicService, times(1)).validateUserPicPickupPoints(pickupPointSetOutput);
      Mockito.verify(userPicService, times(1))
          .validateUserPicPickupPoints(Set.of(PICKUP_POINT_CODE));
      Assertions.assertEquals(REVIEW_TYPE, editProductWebResponse.getReviewType());
      Assertions.assertTrue(editProductWebResponse.isProductReview());
    }
  }

  private static List<ItemPickupPointWebRequest> getItemPickupPointRequests() {
    ItemPickupPointWebRequest itemPickupPointRequest = new ItemPickupPointWebRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    ItemPickupPointWebRequest itemPickupPointRequestTwo = new ItemPickupPointWebRequest();
    itemPickupPointRequestTwo.setPickupPointId(PICKUP_POINT_CODE_TWO);
    itemPickupPointRequestTwo.setItemSku(ITEM_SKU);
    List<ItemPickupPointWebRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    itemPickupPointRequestList.add(itemPickupPointRequestTwo);
    return itemPickupPointRequestList;
  }

  private static List<ItemPickupPointWebRequest> getItemPickupPointRequestForSingleList() {
    ItemPickupPointWebRequest itemPickupPointRequest = new ItemPickupPointWebRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE_THREE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointNotUpdated(true);
    List<ItemPickupPointWebRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    return itemPickupPointRequestList;
  }

  private static List<ItemPickupPointDeleteWebRequest> getDeletePickupPointRequest() {
    ItemPickupPointDeleteWebRequest pickupPointDeleteRequest = new ItemPickupPointDeleteWebRequest();
    pickupPointDeleteRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointDeleteRequest.setItemSku(ITEM_SKU);
    ItemPickupPointDeleteWebRequest pickupPointDeleteRequestTwo = new ItemPickupPointDeleteWebRequest();
    pickupPointDeleteRequestTwo.setPickupPointId(PICKUP_POINT_CODE_TWO);
    pickupPointDeleteRequestTwo.setItemSku(ITEM_SKU);
    List<ItemPickupPointDeleteWebRequest> pickupPointDeleteRequestList = new ArrayList<>();
    pickupPointDeleteRequestList.add(pickupPointDeleteRequest);
    pickupPointDeleteRequestList.add(pickupPointDeleteRequestTwo);
    return pickupPointDeleteRequestList;
  }

  @Test
  public void editProductInfoExceptionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESSPARTNER_CODE)).thenReturn(profileResponse);
    when(this.pbpFeign.editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), true))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try (MockedStatic<RequestHelper> mockedRequestHelper = mockStatic(RequestHelper.class);
        MockedStatic<Credential> mockedCredential = mockStatic(Credential.class)) {
      mockedCredential.when(Credential::getAccessibilities).thenReturn(accessibilities());
      mockedRequestHelper.when(() -> RequestHelper.toProductL3RequestFromProductEditInfoWebRequest(
          productEditInfoWebRequest, IS_WARE_HOUSE)).thenReturn(productLevel3Request);
    try {
      this.productV2Service.editProductV2Info(productEditInfoWebRequest, BUSINESSPARTNER_CODE, true);
    } catch (Exception e) {
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESSPARTNER_CODE);
      Mockito.verify(pbpFeign)
          .editProductV2Info(productLevel3Request, productLevel3Request.getProductSku(), true);
    }
    }
  }

  @Test
  public void fetchL3DetailsByProductSkuTest() throws Exception {
    profileResponse.getCompany().setPurchaseTerm("PO");
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1)).thenReturn(profileResponse);
    when(this.pbpFeign.fetchL3V2ProductDetailsByProductSku(PRODUCT_SKU_1, false))
      .thenReturn(new GdnRestSingleResponse<>(new ProductLevel3DetailsV2Response(), REQUEST_ID));
    this.productV2Service.fetchL3DetailsByProductSku(STORE_ID,PRODUCT_SKU_1, BUSINESS_PARTNER_CODE_1,
      false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    verify(pbpFeign).fetchL3V2ProductDetailsByProductSku(PRODUCT_SKU_1, false);
  }

  @Test
  void fetchL3DetailsByProductSkuExceptionTest() throws Exception {
    profileResponse.setMerchantStatus("INACTIVE");
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
     .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productV2Service.fetchL3DetailsByProductSku(STORE_ID, PRODUCT_SKU_1,
              BUSINESS_PARTNER_CODE_1, false));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode
       (BUSINESS_PARTNER_CODE_1);
      Mockito.verify(pbpFeign, times(0)).fetchL3V2ProductDetailsByProductSku(GDN_SKU, false);
    }
  }

  @Test
  void getL3DetailsByProductSku_nullBusinessPartnerExceptionTest() throws Exception {
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> this.productV2Service.fetchL3DetailsByProductSku(STORE_ID, PRODUCT_SKU_1,
              BUSINESS_PARTNER_CODE_1, false));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
      Mockito.verify(pbpFeign, times(0)).fetchL3V2ProductDetailsByProductSku(GDN_SKU, false);
    }
  }

  @Test
  public void getL3DetailsByProductSkuCogsViewableTest() throws Exception {
    profileResponse.getCompany().setPurchaseTerm("PO");
    ProductLevel3DetailsV2Response productLevel3DetailResponse = new ProductLevel3DetailsV2Response();
    productLevel3DetailResponse.setCategoryId(CATEGORY_CODE);
    when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1)).thenReturn(profileResponse);
    when(this.pbpFeign.fetchL3V2ProductDetailsByProductSku(PRODUCT_SKU_1, false))
      .thenReturn(new GdnRestSingleResponse<>(productLevel3DetailResponse, REQUEST_ID));
    ProductLevel3V2WebResponse productL3DetailWebResponse =
      this.productV2Service.fetchL3DetailsByProductSku(STORE_ID,PRODUCT_SKU_1, BUSINESS_PARTNER_CODE_1,
        false);
    verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    verify(pbpFeign).fetchL3V2ProductDetailsByProductSku(PRODUCT_SKU_1, false);
    Assertions.assertTrue(productL3DetailWebResponse.getCogsViewable());
  }

  @Test
  public void getItemPickupPointListingByProductSkuTest() {
    ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest =
      new ItemPickupPointListingL3WebRequest();
    ItemPickupPointListingL3Request itemPickupPointListingL3Request =
      new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(PRODUCT_SKU);
    productV2Service
      .getItemPickupPointListingByProductSku(0, 1, PRODUCT_SKU, itemPickupPointListingL3WebRequest);
    verify(productService)
      .getItemPickupPointListingByProductSku(0, 1, false, PRODUCT_SKU, itemPickupPointListingL3WebRequest, false);
  }

  @Test
  public void fetchBulkProcessListingResponse_ValidInput_ReturnsPage() throws Exception {
    profileResponse.setMerchantStatus("ACTIVE");
    BulkProcessStatusListingWebResponse webResponse =
      BulkProcessStatusListingWebResponse.builder().bulkProcessCode(BULK_PROCESS_CODE)
        .bulkProcessCode(BULK_PROCESS_CODE).bulkProcessType(BULK_PROCESS_TYPE)
        .isProcessCompleted(true).build();
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(bulkProcessService.fetchBulkProcessListingWebResponse(STORE_ID, REQUEST_ID,
      BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE)).thenReturn(
      new PageImpl<>(Collections.singletonList(webResponse), PageRequest.of(PAGE, SIZE),
        pageMetaData.getTotalRecords()));
    Page<BulkProcessStatusListingWebResponse> result =
      productV2Service.fetchBulkProcessListingResponse(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE,
        BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(bulkProcessService)
      .fetchBulkProcessListingWebResponse(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE,
        BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE);
    Assertions.assertNotNull(result);
  }

  @Test
  void fetchBulkProcessListingResponse_BlankBusinessPartnerCode_ThrowsException() throws Exception {
    String blankBusinessPartnerCode = "";
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productV2Service.fetchBulkProcessListingResponse(STORE_ID, REQUEST_ID,
            blankBusinessPartnerCode, BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE));
  }

  @Test
  void fetchBulkProcessListingResponse_NullBulkProcessType_ThrowsException() throws Exception {
    String nullBulkProcessType = null;
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productV2Service.fetchBulkProcessListingResponse(STORE_ID, REQUEST_ID,
            BUSINESS_PARTNER_CODE, nullBulkProcessType, Optional.empty(), false, PAGE, SIZE));
  }

  @Test
  void fetchBulkProcessListingResponse_InvalidBusinessPartnerCode_ThrowsException() throws Exception {
    profileResponse.setMerchantStatus("INACTIVE");
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productV2Service.fetchBulkProcessListingResponse(STORE_ID, REQUEST_ID,
            BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE));
  }

  @Test
  void fetchBulkProcessListingResponse_NullProfileResponse_ThrowsException() throws Exception {
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(null);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productV2Service.fetchBulkProcessListingResponse(STORE_ID, REQUEST_ID,
            BUSINESS_PARTNER_CODE, BULK_PROCESS_TYPE, Optional.empty(), false, PAGE, SIZE));
  }

  @Test
  public void fetchConsignmentDetailResponse_Test() throws Exception{
    ReflectionTestUtils.setField(productV2Service, "consignmentDetailsMaxFetchDays", "1");
    ReflectionTestUtils.setField(productV2Service, "sortByForConsignmentDetailListing",
      "status");
    ReflectionTestUtils.setField(productV2Service,"headerAuthenticatorFbb","111111");
    Response<List<ConsignmentStatusResponse>> consignmentFormsDetail =
      new Response<>();
    consignmentFormsDetail.setPaging(new Paging(PAGE + 1,2,5,7));
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber(ITEM_CODE).build()));
    Calendar calendar = Calendar.getInstance();
    String endDate = new SimpleDateFormat("yyyy-MM-dd").format(calendar.getTime());
    calendar.add(Calendar.DATE, -1);
    String startDate = new SimpleDateFormat("yyyy-MM-dd").format(calendar.getTime());
    Mockito.when(
        fbbFeign.partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC, PAGE + 1, SIZE,
          ITEM_SKU, BUSINESS_PARTNER_CODE, startDate, endDate, "111111"))
      .thenReturn(consignmentFormsDetail);
    Pair<List<ConsignmentDetailWebResponse>, Integer> consignmentDetailResponse =
      productV2Service.fetchConsignmentDetailResponse(STORE_ID, BUSINESS_PARTNER_CODE, ITEM_SKU,
        PAGE, SIZE);
    Mockito.verify(fbbFeign)
      .partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC, PAGE + 1, SIZE, ITEM_SKU,
        BUSINESS_PARTNER_CODE, startDate, endDate, "111111");
    Assertions.assertEquals(consignmentDetailResponse.getRight(), Optional.of(7).get());

  }

  @Test
  void fetchConsignmentDetailResponseForNullItemSKu() throws Exception{
    ReflectionTestUtils.setField(productV2Service, "consignmentDetailsMaxFetchDays", "1");
    ReflectionTestUtils.setField(productV2Service, "sortByForConsignmentDetailListing",
      "status");
    ReflectionTestUtils.setField(productV2Service,"headerAuthenticatorFbb","111111");
    Response<List<ConsignmentStatusResponse>> consignmentFormsDetail =
      new Response<>();
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber(ITEM_CODE).build()));
    Calendar calendar = Calendar.getInstance();
    String endDate = new SimpleDateFormat("yyyy-MM-dd").format(calendar.getTime());
    calendar.add(Calendar.DATE, -1);
    String startDate = new SimpleDateFormat("yyyy-MM-dd").format(calendar.getTime());
    Mockito.when(fbbFeign.partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC,
        PAGE + 1, SIZE, null, BUSINESS_PARTNER_CODE, startDate, endDate, "111111"))
      .thenReturn(consignmentFormsDetail);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productV2Service.fetchConsignmentDetailResponse(STORE_ID, BUSINESS_PARTNER_CODE,
              ITEM_SKU, PAGE, SIZE));
    }
    finally {
      verify(fbbFeign).partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC, PAGE + 1,
        SIZE, ITEM_SKU, BUSINESS_PARTNER_CODE, startDate, endDate, "111111");
    }
  }

  @Test
  void fetchConsignmentDetailResponseForNullItemSKuExceptionCase() throws Exception{
    ReflectionTestUtils.setField(productV2Service, "sortByForConsignmentDetailListing",
      "status");
    ReflectionTestUtils.setField(productV2Service,"headerAuthenticatorFbb","111111");
    Response<List<ConsignmentStatusResponse>> consignmentFormsDetail =
      new Response<>();
    consignmentFormsDetail.setData(Collections.singletonList(
      ConsignmentStatusResponse.builder().status(ProductConsignmentStatus.IN_PROGRESS)
        .createdDate(new Date()).documentNumber(ITEM_CODE).build()));
    Mockito.when(fbbFeign.partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC,
        PAGE + 1, SIZE, null, BUSINESS_PARTNER_CODE, "", "", "111111"))
      .thenReturn(consignmentFormsDetail);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productV2Service.fetchConsignmentDetailResponse(STORE_ID, BUSINESS_PARTNER_CODE,
              ITEM_SKU, PAGE, SIZE));
    }
    finally {
      verify(fbbFeign).partnerConsignmentFormsByFilterForProduct("status", SortOrder.ASC, PAGE + 1,
        SIZE, ITEM_SKU, BUSINESS_PARTNER_CODE, "", "", "111111");
    }
  }

  @Test
  public void getProductCountForProductLimitTest() {
    GdnRestSingleResponse<com.gda.mta.product.dto.response.ProductCountResponse> response =
        new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(new com.gda.mta.product.dto.response.ProductCountResponse(20L));
    Mockito.when(pbpFeign.getProductCountForProductLimit(BUSINESS_PARTNER_CODE)).thenReturn(response);
    productV2Service.getProductCountForProductLimit(DEFAULT_STORE_ID, BUSINESS_PARTNER_CODE);
    verify(pbpFeign).getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getProductCountForProductLimitTest1() {
    GdnRestSingleResponse<com.gda.mta.product.dto.response.ProductCountResponse> response =
        new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setValue(new com.gda.mta.product.dto.response.ProductCountResponse(20L));
    Mockito.when(pbpFeign.getProductCountForProductLimit(BUSINESS_PARTNER_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productV2Service.getProductCountForProductLimit(DEFAULT_STORE_ID,
              BUSINESS_PARTNER_CODE));
    } finally {
      verify(pbpFeign).getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void getProductCountForProductLimitTest2() {
    GdnRestSingleResponse<com.gda.mta.product.dto.response.ProductCountResponse> response =
        new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    Mockito.when(pbpFeign.getProductCountForProductLimit(BUSINESS_PARTNER_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productV2Service.getProductCountForProductLimit(DEFAULT_STORE_ID,
              BUSINESS_PARTNER_CODE));
    } finally {
      verify(pbpFeign).getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void getProductCountForProductLimitExceptionTest() {
    GdnRestSingleResponse<com.gda.mta.product.dto.response.ProductCountResponse> response =
        new GdnRestSingleResponse<>();
    response.setSuccess(false);
    Mockito.when(pbpFeign.getProductCountForProductLimit(BUSINESS_PARTNER_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productV2Service.getProductCountForProductLimit(DEFAULT_STORE_ID,
              BUSINESS_PARTNER_CODE));
    } finally {
      verify(pbpFeign).getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void getProductCountForProductLimitNullResponseExceptionTest() {
    Mockito.when(pbpFeign.getProductCountForProductLimit(BUSINESS_PARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productV2Service.getProductCountForProductLimit(DEFAULT_STORE_ID,
              BUSINESS_PARTNER_CODE));
    } finally {
      verify(pbpFeign).getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void fetchBasicItemDetailsByItemCodesTest(){
    ReflectionTestUtils.setField(productV2Service, "fetchBasicItemDetailsSortBy",
      SORT_BY);
    ReflectionTestUtils.setField(productV2Service, "fetchBasicItemDetailsOrderBy",
      ORDER_BY);
    ReflectionTestUtils.setField(productV2Service, "productDetailPageUrlPrefix",
      PRODUCT_DETAIL_LINK );
    ItemCodeBasicDetailResponse itemCodeBasicDetailResponse =
      ItemCodeBasicDetailResponse.builder().itemName(ITEM_NAME).itemSku(ITEM_SKU)
        .itemCode(ITEM_CODE).itemName(ITEM_NAME).suspended(true).archivedAtL3(true).build();
    GdnRestListResponse<ItemCodeBasicDetailResponse>
      itemCodeBasicDetailResponseGdnRestListResponse =
      new GdnRestListResponse<>(Collections.singletonList(itemCodeBasicDetailResponse),
        pageMetaData, REQUEST_ID);
    itemCodeBasicDetailResponseGdnRestListResponse.setSuccess(true);
    itemCodeBasicDetailResponseGdnRestListResponse.setErrorCode(null);
    itemCodeBasicDetailResponseGdnRestListResponse.setErrorMessage(null);
    Mockito.when(xProductFeign.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME,
      PAGE, SIZE, SORT_BY, ORDER_BY ,ITEM_CODE, ITEM_NAME)).thenReturn(itemCodeBasicDetailResponseGdnRestListResponse);
    productV2Service.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME, ITEM_CODE,
      ITEM_NAME, PAGE, SIZE);
    Mockito.verify(xProductFeign).fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME,
      PAGE, SIZE, SORT_BY, ORDER_BY ,ITEM_CODE, ITEM_NAME);
  }

  @Test
  void fetchBasicItemDetailsByItemCodesExceptionTest() {
    ReflectionTestUtils.setField(productV2Service, "fetchBasicItemDetailsSortBy", SORT_BY);
    ReflectionTestUtils.setField(productV2Service, "fetchBasicItemDetailsOrderBy", ORDER_BY);
    ItemCodeBasicDetailResponse itemCodeBasicDetailResponse =
        ItemCodeBasicDetailResponse.builder().itemName(ITEM_NAME).itemSku(ITEM_SKU).itemCode(ITEM_CODE).itemName(ITEM_NAME).suspended(true).archivedAtL3(true).build();
    GdnRestListResponse<ItemCodeBasicDetailResponse> itemCodeBasicDetailResponseGdnRestListResponse =
        new GdnRestListResponse<>(Collections.singletonList(itemCodeBasicDetailResponse), pageMetaData, REQUEST_ID);
    itemCodeBasicDetailResponseGdnRestListResponse.setSuccess(false);
    itemCodeBasicDetailResponseGdnRestListResponse.setErrorCode(null);
    itemCodeBasicDetailResponseGdnRestListResponse.setErrorMessage(null);
    Mockito.when(
        xProductFeign.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME, PAGE, SIZE, SORT_BY, ORDER_BY,
            ITEM_CODE, ITEM_NAME)).thenReturn(itemCodeBasicDetailResponseGdnRestListResponse);
    try {
      Assertions.assertThrows(ClientException.class,
          () -> productV2Service.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME, ITEM_CODE, ITEM_NAME, PAGE,
          SIZE));
    } finally {
      Mockito.verify(xProductFeign)
          .fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, USER_NAME, PAGE, SIZE, SORT_BY, ORDER_BY, ITEM_CODE,
              ITEM_NAME);
    }
  }

  @Test
  public void getBulkProcessResponseTest() {
    BulkProcessResponse bulkProcessResponse = new BulkProcessResponse();
    bulkProcessResponse.setBulkProcessCode(BULK_PROCESS_CODE);
    Mockito.when(bulkProcessService.getBulkProcessResponseByProcessCode(BULK_PROCESS_CODE))
        .thenReturn(bulkProcessResponse);
    productV2Service.getBulkProcessResponse(BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessService).getBulkProcessResponseByProcessCode(BULK_PROCESS_CODE);
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSku() {
    L2StockDetailResponse l2StockDetailResponse1 =
      L2StockDetailResponse.builder().warehouseItemSku(WAREHOUSE_ITEM_SKU)
        .distributionWarehouseAvailable(false).nonDistributionWarehouseAvailable(false).build();
    Mockito.when(
        xInventoryFeign.findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse1, REQUEST_ID));

    List<String> itemCodes = new ArrayList<String>();
    itemCodes.add(WAREHOUSE_ITEM_SKU);
    productV2Service.fetchWarehouseStockStatusByItemCode(itemCodes);
    Mockito.verify(xInventoryFeign)
      .findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU);
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSku2() {
    L2StockDetailResponse l2StockDetailResponse1 =
      L2StockDetailResponse.builder().warehouseItemSku(WAREHOUSE_ITEM_SKU)
        .distributionWarehouseAvailable(false).nonDistributionWarehouseAvailable(true).build();
    Mockito.when(
        xInventoryFeign.findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse1, REQUEST_ID));

    List<String> itemCodes = new ArrayList<String>();
    itemCodes.add(WAREHOUSE_ITEM_SKU);
    productV2Service.fetchWarehouseStockStatusByItemCode(itemCodes);
    Mockito.verify(xInventoryFeign)
      .findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU);
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSku3() {
    L2StockDetailResponse l2StockDetailResponse =
      L2StockDetailResponse.builder().warehouseItemSku(WAREHOUSE_ITEM_SKU)
        .distributionWarehouseAvailable(true).nonDistributionWarehouseAvailable(false).build();
    Mockito.when(
        xInventoryFeign.findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));

    List<String> itemCodes = new ArrayList<String>();
    itemCodes.add(WAREHOUSE_ITEM_SKU);
    productV2Service.fetchWarehouseStockStatusByItemCode(itemCodes);
    Mockito.verify(xInventoryFeign)
      .findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU);
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSku4() {
    L2StockDetailResponse l2StockDetailResponse1 =
      L2StockDetailResponse.builder().warehouseItemSku(WAREHOUSE_ITEM_SKU)
        .distributionWarehouseAvailable(false).nonDistributionWarehouseAvailable(true).build();
    Mockito.when(
        xInventoryFeign.findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse1, REQUEST_ID));

    List<String> itemCodes = new ArrayList<String>();
    itemCodes.add(WAREHOUSE_ITEM_SKU);
    productV2Service.fetchWarehouseStockStatusByItemCode(itemCodes);
    Mockito.verify(xInventoryFeign)
      .findStockAvailabilityByWarehouseItemSku(WAREHOUSE_ITEM_SKU);
  }

  private String[] accessibilities() {
    String[] accessibilities =
      {Accessibilty.MAINTAIN_PRODUK_SKU_DISPLAY_BUYABLE_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_STOCK_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_HARGA_CHANGE_DATA, Accessibilty.MAINTAIN_PRODUK_SKU_O2O_CHANGE_DATA,
        Accessibilty.MAINTAIN_PRODUK_SKU_PICKUP_POINT_CHANGE_DATA, Accessibilty.FUNCTION_MAINTAIN_PRODUK_SKU_SYNC_STOCK,
        Accessibilty.IS_PRIVILEGED_TO_EDIT_PRICE, Accessibilty.IS_PRIVILEGED_TO_EDIT_AVAILABLE_STOCK};
    return accessibilities;
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerTest() {
    when(pbpFeign.checkOmniChannelSkuExistsInSeller(any(OmniChannelExistsRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new OmniChannelMapAndSkuResponse(), REQUEST_ID));
    productV2Service.checkOmniChannelSkuExistsInSeller(new OmniChannelSkuWebRequest());
    Mockito.verify(pbpFeign).checkOmniChannelSkuExistsInSeller(any(OmniChannelExistsRequest.class));
  }
}
