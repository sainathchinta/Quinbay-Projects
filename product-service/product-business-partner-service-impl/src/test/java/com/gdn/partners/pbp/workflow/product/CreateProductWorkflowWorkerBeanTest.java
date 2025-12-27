package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.B2bDetailsDTO;
import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.repository.BrandWipRepository;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.repository.BrandRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.google.common.collect.ImmutableSet;

public class CreateProductWorkflowWorkerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String GDN_SKU = "gdnSku";
  private static final String LOCAL = "LOCAL";
  private static final String GDN_SKU_1 = "gdnSku1";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ITEM_ID_1 = "productItemId1";
  private static final String PRODUCT_ITEM_CODE_1 = "productItemCode1";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_2 = "pickupPointCode2";
  private static final String PRODUCT_ITEM_CODE = "productItemCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_CODE_2 = "businessPartnerCode2";
  private static final String BUSINESS_PARTNER_ID = "businessPartnerId";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String PROCESS_CODE = "processCode";
  private static final String UPC_CODE = "12345";
  private static final String SOURCE_ITEM_CODE = "SOURCE_ITEM_CODE";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";
  private static final String BRAND_REJECTED_STATUS = "REJECTED";
  private static final String BRAND_DRAFT_STATUS = "DRAFT";
  private static final String VALUE = "value";
  private static final String PATH = "path";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String INTERNAL_USER = "INTERNAL";
  private static final String INVALID_EAN_UPC_FORMAT = "EAN UPC code is not in valid format";
  private static final String ATTRIBUTE_VALUE = "value";
  private static final String ATTRIBUTE_NAME = "name";
  private static final String ATTRIBUTE_CODE = "code";
  private static final String ATTRIBUTE_CODE_1 = "code1";
  private static final String SPECIFICATION_DETAIL = "<ul><li>name<ul><li>value</li></ul></li></ul>";
  private static final String bulkUploadType = "UNIFIED_BULK_UPLOAD";
  private static final String AUTO_UPLOAD = "AUTO_UPLOAD";
  private static final String PREORDER_DAYS_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String USERNAME = "username";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final Double LENGTH = 10.0;
  private static final Double WIDTH = 10.0;
  private static final Double HEIGHT = 10.0;
  private static final Double WEIGHT = 0.167;
  private static final Double WEIGHT_1 = 1.667;
  private static final Double SHIPPING_WEIGHT = 0.01;
  private static final Integer LOGISTIC_ADJ = 100;

  private Map<String, Object> datas;
  private Map<String, Object> wholesalePriceDatas;
  private ProductCreationRequest productCreationRequest;
  private ProductCreationRequest productCreationRequestWithWholesalePrice;
  private BrandResponse brandResponse = new BrandResponse();
  private BrandWipResponse brandWipResponse = new BrandWipResponse();
  private ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
  private AttributeRequest attributeRequest = new AttributeRequest();
  private ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
  private PreOrderRequest preOrderRequest;
  private ProductSystemParameter productSystemParameter;

  @InjectMocks
  private CreateProductWorkflowWorkerBean createProductWorkflowWorkerBean;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private MapperUtil mapperUtil;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>> configurationStatusRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemWholesalePrice>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductBusinessPartner> productBusinessPartnerArgumentCaptor;

  @Captor
  private ArgumentCaptor<Boolean> booleanArgumentCaptor;

  @Mock
  private BrandWipRepository brandWipRepository;

  private List<ConfigurationStatusResponse> configurationStatusResponses;
  private CategoryDetailResponse categoryDetailResponse;
  private CategoryAttributeResponse categoryAttributeResponse;
  private AttributeResponse attributeResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put("username", "developer");
    ConfigurationStatusResponse configurationStatusResponse = new ConfigurationStatusResponse();
    configurationStatusResponse.setCategoryCode(CATEGORY_CODE);
    configurationStatusResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    configurationStatusResponse.setReviewConfig("Post-Live");
    configurationStatusResponses = Arrays.asList(configurationStatusResponse);

    productCreationRequest = generateProductCreationRequest();
    productCreationRequestWithWholesalePrice = generateProductCreationRequestWithWholeSalePrice();
    datas = new HashMap<>();
    datas.put("request", productCreationRequest);
    datas.put("processCode", PROCESS_CODE);
    datas.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());
    wholesalePriceDatas = new HashMap<>();
    wholesalePriceDatas.put("request", productCreationRequestWithWholesalePrice);
    wholesalePriceDatas.put("processCode", PROCESS_CODE);
    wholesalePriceDatas.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());

    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(Boolean.TRUE);

    brandWipResponse.setBrandRequestCode(BRAND_CODE);
    brandWipResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    brandWipResponse.setState(BrandWipState.DRAFT.name());

    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandResponse, Constants.DEFAULT_REQUEST_ID));
    categoryAttributeResponse = new CategoryAttributeResponse();
    attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setName(ATTRIBUTE_NAME);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setActivated(true);
    categoryDetailResponse.setCategoryCode(CATEGORY_CODE);
    categoryDetailResponse.setLogisticAdjustment(LOGISTIC_ADJ);
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse, categoryAttributeResponse));

    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, Constants.DEFAULT_REQUEST_ID));

    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest.setName(ATTRIBUTE_NAME);
    attributeRequest.setSkuValue(false);
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeValueRequest.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    Mockito.when(mandatoryParameterHelper.getUsername())
        .thenReturn(USERNAME);
    Mockito.when(mandatoryParameterHelper.getRequestId())
        .thenReturn(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getClientId())
        .thenReturn(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getChannelId())
        .thenReturn(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    preOrderRequest =
        PreOrderRequest.builder().isPreOrder(true).preOrderType(PREORDER_DAYS_TYPE).preOrderValue(PREORDER_VALUE)
            .build();
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "isSkipReviewSwitch", false);
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "prioritySeller", 1);
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "eanUpcValidLength",
        Arrays.asList(5, 8, 12, 13, 14, 15));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRepository, productLevel1CollectionService, 
        productLevel1HistoryService, productBusinessPartnerService, productWfService, productService, mapperUtil,
        productItemWholesalePriceService, pcbFeign, brandRepository, brandWipRepository);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
  }

  @Test
  public void processWithCategoryAttributesContainsAllrequestAttributes() throws Exception {
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    List<ProductItemCreationRequest> productItemCreationRequests = new ArrayList<>();
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "validateCreateAttribute", true);
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE_1);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productItemRequest.setImages(Arrays.asList(image));
    productItemRequest.setAttributesMap(new TreeMap<>());
    productItemRequest.setUpcCode(UPC_CODE);
    productItemAttributeValueRequest.setValue(VALUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemRequest.setProductItemAttributeValueRequests(Arrays.asList(productItemAttributeValueRequest));
    productItemCreationRequests.add(productItemRequest);
    productCreationRequest.setProductItemRequests(productItemCreationRequests);
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productCreationRequest.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productCreationRequest.setBrand(BRAND_NAME);
    productCreationRequest.setBrandCode(BRAND_CODE);
    productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    productCreationRequest
        .setProductBusinessPartnerAttributes(Arrays.asList(new ProductBusinessPartnerAttributeRequest()));
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequest.setCategory(categoryRequest);
    productCreationRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    ProductCategoryRequest productCategory = new ProductCategoryRequest();
    CategoryRequest category = new CategoryRequest();
    category.setCategoryCode(CATEGORY_CODE);
    productCategory.setCategory(category);
    productCreationRequest.setWeight(WEIGHT);
    productCreationRequest.setHeight(HEIGHT);
    productCreationRequest.setWidth(WIDTH);
    productCreationRequest.setShippingWeight(SHIPPING_WEIGHT);
    productCreationRequest.setLength(LENGTH);
    productCreationRequest.setProductCategories(Arrays.asList(productCategory));
    datas.put("request", productCreationRequest);

    Mockito.when(this.productOutbound.authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(), BUSINESS_PARTNER_CODE))
        .thenReturn(new SimpleBooleanResponse(true));
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    productCollection.setPrioritySeller(1);
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()), PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Mockito
        .when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType)).thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class), Mockito.eq(productDetailResponse),
            Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE), Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class), Mockito.eq(productDetailResponse),
            Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems = productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(0).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(0).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertNull(productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), WEIGHT);
  }

  @Test
  public void process() throws Exception {
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), WEIGHT);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processThrowException() throws Exception {
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "validateCreateAttribute", true);
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    }finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }

  }

  @Test
  public void processMppFlow() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "ranchIntegrationEnabled", true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setPreOrderQuota(10);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(10,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPreOrderQuota(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlow_cncForWarehouseSwitchTrue() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "cncForWarehouseFeatureSwitch",
        true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(false);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    BeanUtils.copyProperties(pickupPointCreateRequest, pickupPointCreateRequest1);
    pickupPointCreateRequest1.setCncDisplay(false);
    pickupPointCreateRequest1.setCncBuyable(true);

    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(
        GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(),
        booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService.create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME,
            productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(
            Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService)
        .create(productCollection, PROCESS_CODE, null);
    Mockito.when(
            productBusinessPartnerService.saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                Mockito.eq(productDetailResponse), Mockito.eq(true)))
        .thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE,
            BRAND_APPROVAL_STATUS, true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository)
        .getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            BRAND_NAME, false, true);
    Mockito.verify(xProductOutbound)
        .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE,
        productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory()
            .getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlow_cncForWarehouseSwitchTrueCncTrue() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "cncForWarehouseFeatureSwitch",
        true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(false);

    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(
        GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(),
        booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService.create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME,
            productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(
            Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService)
        .create(productCollection, PROCESS_CODE, null);
    Mockito.when(
            productBusinessPartnerService.saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                Mockito.eq(productDetailResponse), Mockito.eq(true)))
        .thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE,
            BRAND_APPROVAL_STATUS, true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository)
        .getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE,
        productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory()
            .getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlowCncXProductEmptyResponseTest() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "ranchIntegrationEnabled", true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    productCreationRequest.setWeight(1.0);
    productCreationRequest.setLength(100.0);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setOnline(false);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE));
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertFalse(
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertFalse(productBusinessPartnerArgumentCaptor.getValue().isOnline());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(WEIGHT_1, productRequestArgumentCaptor.getValue().getShippingWeight(), 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
    Assertions.assertEquals(BRAND_NAME, productCreationRequest.getBrand());
  }

  @Test
  public void processMppFlowCncFalseTest() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "ranchIntegrationEnabled", true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setOnline(true);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Map<String, String> distributionInfoMap = new HashMap<>();
    distributionInfoMap.put(PROCESS_CODE, PRODUCT_CODE);
    productCreationRequest.setDistributionInfoRequest(distributionInfoMap);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCncActivated(false);
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse1 = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse1.setCncActivated(true);
    businessPartnerPickupPointResponse1.setCode(PRODUCT_CODE);
    Mockito.when(
        xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Arrays.asList(businessPartnerPickupPointResponse, businessPartnerPickupPointResponse1));
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE));
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertFalse(
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOnline());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getDistributionInfoRequest().isEmpty());
  }

  @Test
  public void processMppFlowCncFalseDistributionInfoTest() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "ranchIntegrationEnabled", true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setOnline(true);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Map<String, String> distributionInfoMap = new HashMap<>();
    distributionInfoMap.put(PROCESS_CODE, PRODUCT_CODE);
    productCreationRequest.setDistributionInfoRequest(distributionInfoMap);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    DistributionItemRequest distributionItemRequest = new DistributionItemRequest();
    distributionItemRequest.setExpiry(true);
    distributionItemRequest.setOrigin(LOCAL);
    distributionItemRequest.setOmniChannelSku(USERNAME);
    productCreationRequest.getProductItemRequests().get(0)
        .setDistributionItemInfoRequest(distributionItemRequest);
    DimensionAndUomRequest dimensionAndUomRequest = new DimensionAndUomRequest();
    dimensionAndUomRequest.setHeight(10.0);
    dimensionAndUomRequest.setLength(10.0);
    dimensionAndUomRequest.setWeight(10.0);
    dimensionAndUomRequest.setWidth(10.0);
    dimensionAndUomRequest.setUomCode(PROCESS_CODE);
    dimensionAndUomRequest.setUomType(PROCESS_CODE);
    dimensionAndUomRequest.setConversion(1.0);
    dimensionAndUomRequest.setUpcEanList(Collections.singleton(UPC_CODE));
    productCreationRequest.getProductItemRequests().get(0).setDimensionsAndUOMRequest(
        Collections.singletonList(dimensionAndUomRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.getProductItemRequests().stream()
        .forEach(productItemCreationRequest -> productItemCreationRequest.setMerchantSku(USERNAME));
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCncActivated(false);
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse1 = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse1.setCncActivated(true);
    businessPartnerPickupPointResponse1.setCode(PRODUCT_CODE);
    Mockito.when(
            xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Arrays.asList(businessPartnerPickupPointResponse, businessPartnerPickupPointResponse1));
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE));
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertFalse(
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOnline());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getDistributionInfoRequest().isEmpty());
    Assertions.assertTrue(productItems.get(0).getProductItemUomInfoDTO().getDistributionItemInfoRequest().isExpiry());
    Assertions.assertEquals(USERNAME,
        productItems.get(0).getProductItemUomInfoDTO().getDistributionItemInfoRequest().getOmniChannelSku());
    Assertions.assertEquals(LOCAL,
        productItems.get(0).getProductItemUomInfoDTO().getDistributionItemInfoRequest().getOrigin());
    Assertions.assertEquals(10.0,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getHeight());
    Assertions.assertEquals(10.0,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getWidth());
    Assertions.assertEquals(10.0,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getLength());
    Assertions.assertEquals(10.0,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getWidth());
    Assertions.assertEquals(1.0,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getConversion());
    Assertions.assertEquals(UPC_CODE,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getUpcEanList().iterator()
            .next());
    Assertions.assertEquals(PROCESS_CODE,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getUomCode());
    Assertions.assertEquals(PROCESS_CODE,
        productItems.get(0).getProductItemUomInfoDTO().getDimensionAndUomDTOList().get(0).getUomType());
  }

  @Test
  public void processMppFlowCncFalseDistributionInfoTest2() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "ranchIntegrationEnabled", true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setOnline(true);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Map<String, String> distributionInfoMap = new HashMap<>();
    distributionInfoMap.put(PROCESS_CODE, PRODUCT_CODE);
    productCreationRequest.setDistributionInfoRequest(distributionInfoMap);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(0)
        .setDistributionItemInfoRequest(new DistributionItemRequest());
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCncActivated(false);
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse1 = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse1.setCncActivated(true);
    businessPartnerPickupPointResponse1.setCode(PRODUCT_CODE);
    Mockito.when(
            xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Arrays.asList(businessPartnerPickupPointResponse, businessPartnerPickupPointResponse1));
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE));
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertFalse(
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOnline());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getDistributionInfoRequest().isEmpty());
  }

  @Test
  public void processMppFlowCncTrueTest() throws Exception {
    datas.put("productCreationType", ProductCreationType.FLOW1_WEB.getProductCreationType());
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setCncActive(false);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Arrays.asList(pickupPointCreateRequest, pickupPointCreateRequest1));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setPrioritySeller(1);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCncActivated(false);
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse1 = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse1.setCncActivated(true);
    businessPartnerPickupPointResponse1.setCode(PICKUP_POINT_CODE);
    Mockito.when(
        xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Arrays.asList(businessPartnerPickupPointResponse, businessPartnerPickupPointResponse1));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, ProductCreationType.FLOW1_WEB.name(), false, 1);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(1, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlowCncTrueTestFalse() throws Exception {
    datas.put("productCreationType", ProductCreationType.FLOW1_WEB.getProductCreationType());
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
            PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncActive(true);
    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setCncActive(false);
    productCreationRequest.getProductItemRequests().get(0)
            .setPickupPoints(Arrays.asList(pickupPointCreateRequest, pickupPointCreateRequest1));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
            .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.setPrioritySeller(1);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
            .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
                    BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
            .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
            .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
                    .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                            false, bulkUploadType))
            .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
            .thenReturn(configurationStatusResponses);
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
            .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                    Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCncActivated(false);
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse1 = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse1.setCncActivated(true);
    businessPartnerPickupPointResponse1.setCode(PICKUP_POINT_CODE);
    Mockito.when(
                    xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
            .thenReturn(Arrays.asList(businessPartnerPickupPointResponse, businessPartnerPickupPointResponse1));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                    true, false, ProductCreationType.FLOW1_WEB.name(), false, 1);
        Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
            .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
                    Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
            .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
                    Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
            .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                    Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
            .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                    Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
            .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
                    GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
                    BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(
            Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
            productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
            productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(
            productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).isCncActivated());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
  }

  @Test
  public void processMppFlowWholeSaleRequest() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(2);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest));

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(2);
    productItemWholesalePriceRequest1.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest1));

    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productItemResponse.setSkuCode(PRODUCT_ITEM_CODE);
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(PRODUCT_ITEM_ID_1);
    productItemResponse1.setSkuCode(PRODUCT_ITEM_CODE_1);
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setGdnProductItemSku(GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemBusinessPartner1.setGdnProductItemSku(GDN_SKU_1);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    Map treeMap = new TreeMap();
    treeMap.put("COLOUR", "BLUE");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID);
    treeMap.clear();
    treeMap.put("COLOUR", "BLACK");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID_1);

    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(productBusinessPartner);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(productItemWholesalePriceService).saveWholesalePrice(listArgumentCaptor.capture());
    Mockito.verify(mapperUtil).mapRequestToString(Mockito.any());
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlowWholeSaleTrueRequest() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(2);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest));

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(2);
    productItemWholesalePriceRequest1.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest1));

    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productItemResponse.setSkuCode(PRODUCT_ITEM_CODE);
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(PRODUCT_ITEM_ID_1);
    productItemResponse1.setSkuCode(PRODUCT_ITEM_CODE_1);
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setGdnProductItemSku(GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemBusinessPartner1.setGdnProductItemSku(GDN_SKU_1);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    Map treeMap = new TreeMap();
    treeMap.put("COLOUR", "BLUE");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID);
    treeMap.clear();
    treeMap.put("COLOUR", "BLACK");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID_1);

    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(productBusinessPartner);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    Mockito.verify(productItemWholesalePriceService).saveWholesalePrice(listArgumentCaptor.capture());
    Mockito.verify(mapperUtil).mapRequestToString(Mockito.any());
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlowWholeSaleTrue1Request() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(2);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest));

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(2);
    productItemWholesalePriceRequest1.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest1));

    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(PRODUCT_ITEM_ID_1);
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setGdnProductItemSku(GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemBusinessPartner1.setGdnProductItemSku(GDN_SKU_1);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    Map treeMap = new TreeMap();
    treeMap.put("COLOUR", "BLUE");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID);
    treeMap.clear();
    treeMap.put("COLOUR", "BLACK");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID_1);

    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(productBusinessPartner);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlowWholeSaleTrue2Request() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(2);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest));

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPrice(100.0);
    pickupPointCreateRequest1.setSalePrice(10.0);
    pickupPointCreateRequest1.setStock(5);
    pickupPointCreateRequest1.setMinimumStock(50);
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest1.setB2bFields(new B2bDetailsDTO());
    pickupPointCreateRequest1.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(2);
    productItemWholesalePriceRequest1.setWholesaleDiscount(10);
    pickupPointCreateRequest.setProductItemWholesalePriceRequests(
        Collections.singletonList(productItemWholesalePriceRequest1));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(productBusinessPartner);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processTest_draftBrand() throws Exception {
    datas.replace("productCreationType", ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType());
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    preOrderRequest.setIsPreOrder(false);
    productCreationRequest.setPreOrder(preOrderRequest);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            true, false, ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType(), false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processTest_rejectedBrand() throws Exception {
    brandWipResponse.setState(BrandWipState.REJECTED.name());
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
          .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
      Mockito.verify(this.pcbFeign)
          .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    }
  }

  @Test
  public void processTest_errorFetchingDraftBrand() throws Exception {
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
          .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
      Mockito.verify(this.pcbFeign)
          .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    }
  }

  @Test
  public void processTest_draftBrandFromDifferentMerchant() throws Exception {
    brandWipResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
          .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
      Mockito.verify(this.pcbFeign)
          .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    }
  }

  @Test
  public void processWithInternalUser() throws Exception {
    productCreationRequest.setBusinessPartnerCode(INTERNAL_USER);
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()), PRODUCT_ITEM_ID);
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new ProductDetailResponse());
    Mockito.when(productLevel1CollectionService
        .create(INTERNAL_USER, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false, bulkUploadType,
            false, 0))
        .thenReturn(productCollection);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(INTERNAL_USER, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false, bulkUploadType,
            false, 0);
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE), Mockito.eq(null));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    List<ProductItemRequest> productItems = productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processWithoutInternalUserAndWithPriority() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "prioritySeller", 1);
    productCreationRequest.setPrioritySeller(1);
    productCreationRequest.setBusinessPartnerCode("");
    productCreationRequest.setCreatedMerchant(BUSINESS_PARTNER_CODE);
    datas.put("request", productCreationRequest);
    datas.put("productCreationType", "");
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()), PRODUCT_ITEM_ID);
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new ProductDetailResponse());
    Mockito.when(productLevel1CollectionService
        .create(INTERNAL_USER, INTERNAL_USER, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false,"",
            false, 1))
        .thenReturn(productCollection);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(INTERNAL_USER, INTERNAL_USER, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false,"",
            false, 1);
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE), Mockito.eq(null));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    List<ProductItemRequest> productItems = productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
  }

  @Test
  public void processWithoutInternalUser() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean,"prioritySeller",0);
    productCreationRequest.setBusinessPartnerCode("");
    productCreationRequest.setCreatedMerchant(BUSINESS_PARTNER_CODE);
    datas.put("request", productCreationRequest);
    datas.put("productCreationType", "");
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()), PRODUCT_ITEM_ID);
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
            .thenReturn(new ProductDetailResponse());
    Mockito.when(productLevel1CollectionService
                    .create(INTERNAL_USER, INTERNAL_USER, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false,"",
                            false, 0))
            .thenReturn(productCollection);
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
            .create(INTERNAL_USER, INTERNAL_USER, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, false,"",
                    false, 0);
    Mockito.verify(productLevel1HistoryService)
            .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE), Mockito.eq(null));
    Mockito.verify(this.pcbFeign)
            .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                    Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
            .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                    Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    List<ProductItemRequest> productItems = productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
            productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
  }

  @Test
  public void processWithWrongUPCCodde() throws Exception {
    productCreationRequest.setBusinessPartnerCode(INTERNAL_USER);
    productCreationRequest.getProductItemRequests().get(0).setUpcCode("1234567");
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()), PRODUCT_ITEM_ID);
    try {
      createProductWorkflowWorkerBean.process(datas);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
          .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
      Assertions.assertTrue(e.getErrorMessage().contains(INVALID_EAN_UPC_FORMAT));
    }
  }


  ProductCreationRequest generateProductCreationRequest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    Image image = new Image();
    image.setLocationPath(PATH);
    productItemRequest.setImages(Arrays.asList(image));
    productItemRequest.setContentChanged(true);
    productItemRequest.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemRequest.setAttributesMap(new TreeMap<>());
    productItemRequest.setUpcCode(UPC_CODE);
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setValue(VALUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemRequest.setProductItemAttributeValueRequests(Arrays
        .asList(productItemAttributeValueRequest));
    List<ProductItemCreationRequest> productItemCreationRequests = new ArrayList<>();
    productItemCreationRequests.add(productItemRequest);
    productItemRequest = new ProductItemCreationRequest();
    productItemRequest.setImages(Arrays.asList(image));
    productItemRequest.setAttributesMap(new TreeMap<>());
    productItemRequest.setUpcCode(UPC_CODE);
    productItemAttributeValueRequest.setValue(VALUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemRequest.setProductItemAttributeValueRequests(Arrays
        .asList(productItemAttributeValueRequest));
    productItemCreationRequests.add(productItemRequest);
    productCreationRequest.setProductItemRequests(productItemCreationRequests);
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productCreationRequest.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productCreationRequest.setBrand(BRAND_NAME);
    productCreationRequest.setBrandCode(BRAND_CODE);
    productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Arrays.asList(new ProductBusinessPartnerAttributeRequest()));
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequest.setCategory(categoryRequest);
    productCreationRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    ProductCategoryRequest productCategory = new ProductCategoryRequest();
    CategoryRequest category = new CategoryRequest();
    category.setCategoryCode(CATEGORY_CODE);
    productCategory.setCategory(category);
    productCreationRequest.setWeight(WEIGHT);
    productCreationRequest.setHeight(HEIGHT);
    productCreationRequest.setWidth(WIDTH);
    productCreationRequest.setShippingWeight(SHIPPING_WEIGHT);
    productCreationRequest.setLength(LENGTH);
    productCreationRequest.setProductCategories(Arrays.asList(productCategory));
    return productCreationRequest;
  }

  @Test
  public void processWithWholesalePrices() throws Exception {
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productItemResponse.setSkuCode(PRODUCT_ITEM_CODE);
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(PRODUCT_ITEM_ID_1);
    productItemResponse1.setSkuCode(PRODUCT_ITEM_CODE_1);
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setGdnProductItemSku(GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID_1);
    productItemBusinessPartner1.setGdnProductItemSku(GDN_SKU_1);
    productBusinessPartner
        .setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    ProductCollection productCollection = new ProductCollection();
    Map treeMap = new TreeMap();
    treeMap.put("COLOUR", "BLUE");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID);
    treeMap.clear();
    treeMap.put("COLOUR", "BLACK");
    map.put(Base64.encodeBase64String(treeMap.toString().getBytes()),
        PRODUCT_ITEM_ID_1);
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(productBusinessPartner);
    Mockito.when(mapperUtil.mapRequestToString(Mockito.any())).thenReturn(VALUE);
    Mockito.doNothing().when(productItemWholesalePriceService).saveWholesalePrice(Mockito.anyList());
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    createProductWorkflowWorkerBean.process(wholesalePriceDatas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(mapperUtil, Mockito.times(2)).mapRequestToString(Mockito.any());
    Mockito.verify(productItemWholesalePriceService).saveWholesalePrice(listArgumentCaptor.capture());
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.productOutbound)
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(2, listArgumentCaptor.getValue().size());
    Assertions.assertEquals(GDN_SKU, listArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PRODUCT_ITEM_CODE, listArgumentCaptor.getValue().get(0).getItemCode());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  ProductCreationRequest generateProductCreationRequestWithWholeSalePrice() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    Image image = new Image();
    image.setLocationPath(PATH);
    productItemRequest.setImages(Arrays.asList(image));
    Map treeMap = new TreeMap();
    treeMap.put("COLOUR", "BLUE");
    Map treeMap1 = new TreeMap();
    treeMap1.put("COLOUR", "BLACK");
    productItemRequest.setAttributesMap((TreeMap<String, String>) treeMap);
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setValue(VALUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemRequest.setProductItemAttributeValueRequests(Arrays
        .asList(productItemAttributeValueRequest));
    productItemRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(2);
    productItemWholesalePriceRequest.setWholesaleDiscount(20);
    productItemRequest.setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest));
    List<ProductItemCreationRequest> productItemCreationRequests = new ArrayList<>();
    productItemCreationRequests.add(productItemRequest);
    productItemRequest = new ProductItemCreationRequest();
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(5);
    productItemWholesalePriceRequest1.setWholesaleDiscount(30);
    productItemRequest.setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest1));
    productItemRequest.setImages(Arrays.asList(image));
    productItemRequest.setAttributesMap(new TreeMap<>());
    productItemRequest.setUpcCode(UPC_CODE);
    productItemRequest.setAttributesMap((TreeMap<String, String>) treeMap1);
    productItemAttributeValueRequest.setValue(VALUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemRequest.setProductItemAttributeValueRequests(Arrays
        .asList(productItemAttributeValueRequest));
    productItemCreationRequests.add(productItemRequest);
    productCreationRequest.setProductItemRequests(productItemCreationRequests);
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productCreationRequest.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productCreationRequest.setBrandCode(BRAND_CODE);
    productCreationRequest.setBrand(BRAND_NAME);
    productCreationRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    productCreationRequest.setProductBusinessPartnerAttributes(
        Arrays.asList(new ProductBusinessPartnerAttributeRequest()));
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryRequest.setCategory(categoryRequest);
    productCreationRequest.setProductCategories(Arrays.asList(productCategoryRequest));
    ProductCategoryRequest productCategory = new ProductCategoryRequest();
    CategoryRequest category = new CategoryRequest();
    category.setCategoryCode(CATEGORY_CODE);
    productCategory.setCategory(category);
    productCreationRequest.setProductCategories(Arrays.asList(productCategory));
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    return productCreationRequest;
  }

  @Test
  public void processTest_SkipReview() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "isSkipReviewSwitch", true);
    datas.replace("productCreationType", ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType());
    productCreationRequest.getProductItemRequests().get(0).setContentChanged(false);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            true, true, ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType(), false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertFalse(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processTest_SkipReviewAttributeNameNotEmptyTest() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "isSkipReviewSwitch", true);
    datas.replace("productCreationType", ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType());
    productCreationRequest.getProductItemRequests().get(0).setContentChanged(false);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));
    datas.put("request", productCreationRequest);
    productAttributeRequest.getAttribute().setAttributeCode(ATTRIBUTE_CODE_1);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
            .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            true, true, ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType(), false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertFalse(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processTest_SkipReviewFalseTest() throws Exception {
    datas.replace("productCreationType", ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType());
    productCreationRequest.getProductItemRequests().get(0).setContentChanged(false);
    productCreationRequest.setImagesUpdated(true);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));
    productCreationRequest.getProductItemRequests().forEach(productItemRequest -> productItemRequest.getPickupPoints()
        .forEach(pickupPointCreateRequest -> pickupPointCreateRequest.setCncActive(false)));
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    brandWipResponse.setBrandName(BRAND_NAME);
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            true, false, ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType(), false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertFalse(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
    Assertions.assertEquals(BRAND_NAME, productCreationRequest.getBrand());
  }

  @Test
  public void processTest_SkipReviewFalseAutoAuploadTest() throws Exception {
    datas.replace("productCreationType", ProductCreationType.AUTO_UPLOAD.getProductCreationType());
    productCreationRequest.getProductItemRequests().get(0).setContentChanged(false);
    productCreationRequest.setImagesUpdated(true);
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true)).thenReturn(null);
    Mockito.when(this.pcbFeign
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, brandWipResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            false, AUTO_UPLOAD))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_DRAFT_STATUS,
            true, false, ProductCreationType.AUTO_UPLOAD.getProductCreationType(), false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertFalse(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processInactiveCategoryTest() throws Exception {
    categoryDetailResponse.setActivated(false);
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void processExceptionTest() throws Exception {
    categoryDetailResponse.setActivated(false);
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenThrow(RuntimeException.class);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void processInactiveCategoryNullTest() throws Exception {
    categoryDetailResponse.setActivated(false);
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void processInactiveCategoryNull1Test() throws Exception {
    categoryDetailResponse.setActivated(false);
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(datas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void validateProtectedBrandExceptionTest() throws Exception {
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(wholesalePriceDatas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND_NAME, false, true);
      Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
          BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void validateProtectedBrand_pbpExceptionTest() throws Exception {
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(false));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        createProductWorkflowWorkerBean.process(wholesalePriceDatas);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          CATEGORY_CODE);
      Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND_NAME, false, true);
      Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
          BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void processBundleRecipeTest() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.getProductItemRequests().forEach(
        productItemCreationRequest -> productItemCreationRequest.setBundleRecipe(
            ImmutableSet.of(new BundleRecipeRequest(GDN_SKU, 1))));
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any(BundleRecipeRequest.class))).thenReturn(StringUtils.EMPTY);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processBundleRecipeErrorTest() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.getProductItemRequests().forEach(
        productItemCreationRequest -> productItemCreationRequest.setBundleRecipe(
            ImmutableSet.of(new BundleRecipeRequest(GDN_SKU, 1))));
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenThrow(
        JsonProcessingException.class);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);
    
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processExternalBulkUploadTest() throws Exception {
    datas.replace("productCreationType", ProductCreationType.EXTERNAL_BULK_UPLOAD.getProductCreationType());
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    datas.put("request", productCreationRequest);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(), BUSINESS_PARTNER_CODE))
        .thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false))).thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, ProductCreationType.EXTERNAL_BULK_UPLOAD.getProductCreationType(), false, 0);
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(false));
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(), BUSINESS_PARTNER_CODE);
    ProductRequest capturedProductRequest = productRequestArgumentCaptor.getValue();
    Assertions.assertTrue(capturedProductRequest.isIgnoreMissingItems());
    Assertions.assertNotNull(capturedProductRequest.getAiGeneratedFieldsResponse());
    Assertions.assertTrue(capturedProductRequest.getAiGeneratedFieldsResponse().isAiGeneratedCategory());
    Assertions.assertTrue(capturedProductRequest.getAiGeneratedFieldsResponse().isAiGeneratedBrand());
  }

  @Test
  public void processBundleRecipeErrorTest2() throws Exception {
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    PickupPointCreateRequest pickupPointCreateRequest2 = new PickupPointCreateRequest();
    pickupPointCreateRequest2.setPrice(100.0);
    pickupPointCreateRequest2.setSalePrice(10.0);
    pickupPointCreateRequest2.setStock(5);
    pickupPointCreateRequest2.setMinimumStock(null);
    pickupPointCreateRequest2.setPickupPointId(PICKUP_POINT_CODE_2);
    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Arrays.asList(pickupPointCreateRequest2, pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1).setPickupPoints(Collections
        .singletonList(pickupPointCreateRequest));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    productCreationRequest.getProductItemRequests().forEach(
        productItemCreationRequest -> productItemCreationRequest.setBundleRecipe(
            ImmutableSet.of(new BundleRecipeRequest(GDN_SKU, 1))));
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture()))
        .thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService
            .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
                false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService).create(productCollection, PROCESS_CODE, null);
    Mockito.when(productBusinessPartnerService
        .saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
            Mockito.eq(productDetailResponse), Mockito.eq(true))).thenReturn(new ProductBusinessPartner());
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenThrow(
        JsonProcessingException.class);
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository).createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS,
            true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE, productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(1).getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(1).getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(1).getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(1).getStock(), 0);
    Assertions.assertEquals(0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(1).getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE, productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlow_cncForWarehouseSwitch_b2bConfigTrue() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "cncForWarehouseFeatureSwitch",
        true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(true);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    BeanUtils.copyProperties(pickupPointCreateRequest, pickupPointCreateRequest1);
    pickupPointCreateRequest1.setCncDisplay(false);
    pickupPointCreateRequest1.setCncBuyable(true);

    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(
        GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(),
        booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService.create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME,
            productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(
            Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService)
        .create(productCollection, PROCESS_CODE, null);
    Mockito.when(
            productBusinessPartnerService.saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                Mockito.eq(productDetailResponse), Mockito.eq(true)))
        .thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE,
            BRAND_APPROVAL_STATUS, true, false, bulkUploadType, false, 0);
    Mockito.verify(xProductOutbound)
        .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(this.productRepository)
        .getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE,
        productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory()
            .getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlow_cncForWarehouseSwitch_onlineConfigTrue() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "cncForWarehouseFeatureSwitch",
        true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncDisplay(true);
    pickupPointCreateRequest.setCncBuyable(true);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    BeanUtils.copyProperties(pickupPointCreateRequest, pickupPointCreateRequest1);
    pickupPointCreateRequest1.setCncDisplay(false);
    pickupPointCreateRequest1.setCncBuyable(true);

    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(
        GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(),
        booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService.create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME,
            productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(
            Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService)
        .create(productCollection, PROCESS_CODE, null);
    Mockito.when(
            productBusinessPartnerService.saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                Mockito.eq(productDetailResponse), Mockito.eq(true)))
        .thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE,
            BRAND_APPROVAL_STATUS, true, false, bulkUploadType, false, 0);
    Mockito.verify(xProductOutbound)
        .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(this.productRepository)
        .getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE,
        productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory()
            .getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void processMppFlow_cncForWarehouseSwitch_pickupPointNonCncTrue() throws Exception {
    ReflectionTestUtils.setField(createProductWorkflowWorkerBean, "cncForWarehouseFeatureSwitch",
        true);
    datas.put("MPPFlow", Boolean.TRUE);
    Map<String, String> map = new HashMap<>();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<>());
    ProductCollection productCollection = new ProductCollection();
    map.put(Base64.encodeBase64String(new TreeMap<String, String>().toString().getBytes()),
        PRODUCT_ITEM_ID);
    productCreationRequest.setPreOrder(preOrderRequest);
    productCreationRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(100.0);
    pickupPointCreateRequest.setSalePrice(10.0);
    pickupPointCreateRequest.setStock(5);
    pickupPointCreateRequest.setMinimumStock(50);
    pickupPointCreateRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointCreateRequest.setCncDisplay(false);
    pickupPointCreateRequest.setCncBuyable(true);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    BeanUtils.copyProperties(pickupPointCreateRequest, pickupPointCreateRequest1);
    pickupPointCreateRequest1.setCncDisplay(false);
    pickupPointCreateRequest1.setCncBuyable(true);

    productCreationRequest.setWeight(0.0);
    productCreationRequest.setHeight(0.0);
    productCreationRequest.setWidth(0.0);
    productCreationRequest.setShippingWeight(0.0);
    productCreationRequest.setLength(0.0);
    productCreationRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    productCreationRequest.getProductItemRequests().get(1)
        .setPickupPoints(Collections.singletonList(pickupPointCreateRequest1));
    productCreationRequest.setFreeSample(true);
    productCreationRequest.setOff2OnChannelActive(true);
    Image image = new Image();
    Image image1 = new Image();
    image.setLocationPath(PICKUP_POINT_CODE);
    image.setMainImages(true);
    image.setCommonImage(true);
    image1.setCommonImage(true);
    image1.setLocationPath(PICKUP_POINT_CODE);
    productCreationRequest.setCommonImages(Arrays.asList(image, image1));
    datas.put("request", productCreationRequest);
    Mockito.when(this.productOutbound.authoriseProtectedBrand(
        GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse =
        BusinessPartnerPickupPointResponse.builder().cncActivated(false).build();
    Mockito.when(
            xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Arrays.asList(businessPartnerPickupPointResponse));
    Mockito.when(productRepository.createProduct(productRequestArgumentCaptor.capture(),
        booleanArgumentCaptor.capture())).thenReturn(map);
    Mockito.when(productRepository.findProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productDetailResponse);
    Mockito.when(productLevel1CollectionService.create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME,
            productDetailResponse, BRAND_CODE, BRAND_APPROVAL_STATUS, false, bulkUploadType))
        .thenReturn(productCollection);
    Mockito.when(this.productRepository.getConfigurationStatus(
            Mockito.anyList()))
        .thenReturn(configurationStatusResponses);

    Mockito.doNothing().when(productLevel1HistoryService)
        .create(productCollection, PROCESS_CODE, null);
    Mockito.when(
            productBusinessPartnerService.saveBusinessPartner(Mockito.any(ProductBusinessPartner.class),
                Mockito.eq(productDetailResponse), Mockito.eq(true)))
        .thenReturn(new ProductBusinessPartner());
    createProductWorkflowWorkerBean.process(datas);
    Mockito.verify(productRepository)
        .createProduct(productRequestArgumentCaptor.capture(), booleanArgumentCaptor.capture());
    Mockito.verify(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productLevel1CollectionService)
        .create(BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, productDetailResponse, BRAND_CODE,
            BRAND_APPROVAL_STATUS, true, false, bulkUploadType, false, 0);

    Mockito.verify(this.productRepository)
        .getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(productLevel1HistoryService)
        .create(productCollectionArgumentCaptor.capture(), Mockito.eq(PROCESS_CODE),
            Mockito.eq(null));
    Mockito.verify(productBusinessPartnerService)
        .saveBusinessPartner(productBusinessPartnerArgumentCaptor.capture(),
            Mockito.eq(productDetailResponse), Mockito.eq(true));
    Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            BRAND_NAME, false, true);
    Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
            GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
            BUSINESS_PARTNER_CODE);
    List<ProductItemRequest> productItems =
        productRequestArgumentCaptor.getValue().getProductItems();
    Assertions.assertEquals(UPC_CODE, productItems.get(1).getUpcCode());
    Assertions.assertNotNull(productItems.get(0).getImages().get(0).getHashCode());
    Assertions.assertNotNull(productItems.get(1).getImages().get(0).getHashCode());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(1).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(Boolean.TRUE, productItems.get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(VALUE,
        productItems.get(1).getProductItemAttributeValues().get(0).getValue());
    Assertions.assertTrue(productRequestArgumentCaptor.getValue().isIgnoreMissingItems());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestArgumentCaptor.getValue().getProductCategories().get(0).getCategory()
            .getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        productRequestArgumentCaptor.getValue().getCreatedMerchant());
    Assertions.assertEquals(SOURCE_ITEM_CODE, productItems.get(0).getSourceItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPickupPointId());
    Assertions.assertEquals(100.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(10.0,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(5,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getStock(), 0);
    Assertions.assertEquals(50,
        productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners().get(0)
            .getMinimumStock(), 0);
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isFreeSample());
    Assertions.assertTrue(productBusinessPartnerArgumentCaptor.getValue().isOff2OnChannelActive());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).getLocationPath());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isMainImages());
    Assertions.assertTrue(
        productRequestArgumentCaptor.getValue().getCommonImages().get(0).isCommonImage());
    Mockito.verify(xProductOutbound)
        .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertTrue(productItems.get(0).isContentChanged());
    Assertions.assertNull(productItems.get(1).getSourceItemCode());
    Assertions.assertFalse(productItems.get(1).isContentChanged());
    Assertions.assertEquals(productRequestArgumentCaptor.getValue().getShippingWeight(), 0.0, 0);
    Assertions.assertEquals(0, productCreationRequest.getPrioritySeller());
  }

  @Test
  public void shouldFetchFromRepository_whenBrandCodeIsEmpty() throws Exception {
    String brand = "TestBrand";
    String bpCode = "BP-123";

    BrandWipResponse wip = new BrandWipResponse();
    wip.setBrandName(brand);
    wip.setBusinessPartnerCode(bpCode);

    Mockito.when(brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(brand, bpCode))
        .thenReturn(wip);

    BrandWipResponse result = createProductWorkflowWorkerBean
        .getBrandWipResponseAndValidateBrandStatus("", brand, bpCode);

    Assertions.assertNotNull(result);
    Assertions.assertEquals(brand, result.getBrandName());

    Mockito.verify(brandWipRepository).findBrandWipByBrandNameAndBusinessPartnerCode(brand, bpCode);
    Mockito.verifyNoInteractions(pcbFeign);
  }

  @Test
  public void shouldThrowDataNotFound_whenFeignResponseIsNull() {
    String brandCode = "BR-111";
    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(null);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode, null, "BP-1")
    );

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldThrowDataNotFound_whenFeignResponseNotSuccess() {
    String brandCode = "BR-112";
    GdnRestSingleResponse<BrandWipResponse> resp = new GdnRestSingleResponse<>();
    resp.setSuccess(false);

    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(resp);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode, null, "BP-1")
    );

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldThrowDataNotFound_whenFeignResponseValueNull() {
    String brandCode = "BR-113";
    GdnRestSingleResponse<BrandWipResponse> resp = new GdnRestSingleResponse<>();
    resp.setSuccess(true);
    resp.setValue(null);

    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(resp);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode, null, "BP-1")
    );

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldThrowValidation_whenRepositoryReturnsNull() throws Exception {
    String brand = "TestBrand";
    String bpCode = "BP-123";

    Mockito.when(brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(brand, bpCode))
        .thenReturn(null);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus("", brand, bpCode)
    );

    Mockito.verify(brandWipRepository).findBrandWipByBrandNameAndBusinessPartnerCode(brand, bpCode);
    Mockito.verifyNoInteractions(pcbFeign);
  }

  @Test
  public void shouldThrowValidation_whenBrandIsRejected() {
    String brandCode = "BR-114";
    BrandWipResponse wip = new BrandWipResponse();
    wip.setState(BRAND_REJECTED_STATUS);

    GdnRestSingleResponse<BrandWipResponse> resp = new GdnRestSingleResponse<>();
    resp.setSuccess(true);
    resp.setValue(wip);

    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(resp);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode, null, "BP-1")
    );

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldThrowValidation_whenBusinessPartnerCodeMismatch() {
    String brandCode = "BR-115";
    BrandWipResponse wip = new BrandWipResponse();
    wip.setBusinessPartnerCode("DIFFERENT-BP");

    GdnRestSingleResponse<BrandWipResponse> resp = new GdnRestSingleResponse<>();
    resp.setSuccess(true);
    resp.setValue(wip);

    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(resp);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode, null, "BP-1")
    );

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldReturnBrandWipResponse_whenEverythingValid() throws Exception {
    String brandCode = "BR-116";
    String bpCode = "BP-123";
    BrandWipResponse wip = new BrandWipResponse();
    wip.setBrandName("ValidBrand");
    wip.setBusinessPartnerCode(bpCode);
    wip.setState("APPROVED");

    GdnRestSingleResponse<BrandWipResponse> resp = new GdnRestSingleResponse<>();
    resp.setSuccess(true);
    resp.setValue(wip);

    Mockito.when(pcbFeign.getBrandWipDetail(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode)))
        .thenReturn(resp);

    BrandWipResponse result = createProductWorkflowWorkerBean
        .getBrandWipResponseAndValidateBrandStatus(brandCode, null, bpCode);

    Assertions.assertNotNull(result);
    Assertions.assertEquals("ValidBrand", result.getBrandName());
    Assertions.assertEquals(bpCode, result.getBusinessPartnerCode());

    Mockito.verify(pcbFeign).getBrandWipDetail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(brandCode));
    Mockito.verifyNoInteractions(brandWipRepository);
  }

  @Test
  public void shouldReturn_whenBusinessPartnerCodeEmpty() throws Exception {
    // Arrange
    String brand = "SomeBrand";
    String requestBp = "BP-456";

    BrandWipResponse wip = new BrandWipResponse();
    wip.setBrandName("BrandNoBp");
    wip.setBusinessPartnerCode("");                   // empty -> isNotEmpty == false
    wip.setState(BrandApprovalStatus.APPROVED.name());

    // Branch: repository path (brandCode empty)
    Mockito.when(brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(brand, requestBp))
        .thenReturn(wip);

    // Act
    BrandWipResponse result = createProductWorkflowWorkerBean
        .getBrandWipResponseAndValidateBrandStatus("", brand, requestBp);

    // Assert
    Assertions.assertNotNull(result);
    Assertions.assertEquals("BrandNoBp", result.getBrandName());

    // Verify: repository used, Feign not called
    Mockito.verify(brandWipRepository).findBrandWipByBrandNameAndBusinessPartnerCode(brand, requestBp);
    Mockito.verifyNoInteractions(pcbFeign);
  }


}
