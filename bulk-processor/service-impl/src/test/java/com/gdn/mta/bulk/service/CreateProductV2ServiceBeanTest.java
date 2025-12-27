package com.gdn.mta.bulk.service;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.QueueHistoryResultDTO;
import com.gdn.mta.bulk.dto.WarningMessage;
import com.gdn.mta.bulk.dto.WarningMessageDTO;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.mta.bulk.dto.product.AllowedValueDtoResponse;
import com.gdn.mta.bulk.dto.product.CreateProductV2Response;
import com.gdn.mta.bulk.dto.product.ProductItemV2Request;
import com.gdn.mta.bulk.dto.product.ProductV2Request;
import com.gdn.mta.bulk.dto.product.ProductWholesaleRequest;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.repository.pcb.ProductAttributeRepository;
import com.gdn.mta.bulk.util.CreateProductV2Util;
import com.gdn.mta.bulk.util.FileManager;
import com.gdn.mta.bulk.util.ImageUtil;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.ProductImageValidator;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.common.io.Files;
import org.springframework.test.util.ReflectionTestUtils;

public class CreateProductV2ServiceBeanTest {

  @InjectMocks
  private CreateProductV2ServiceBean createProductService;
  @Mock
  private ProductRepository productRepository;
  @Mock
  private BusinessPartnerRepository bpRepository;
  @Mock
  private CategoryRepository categoryRepository;
  @Mock
  private GeneratorRepository generatorRepository;
  @Mock
  private SystemParameter systemParameter;
  @Mock
  private KafkaPublisher kafkaProducer;
  @Mock
  private ProductImageValidator imageValidator;
  @Mock
  private TrackerService trackerService;
  @Mock
  private ImageUtil imageUtil;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private ProductAttributeRepository productAttributeRepository;
  @Mock
  private FileStorageService fileStorageService;
  @Mock
  private PickupPointService pickupPointService;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<QueueHistoryResultDTO> argCaptorQueueHistoryResultDTO;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  private PickupPointResponse pickupPointResponse;

  private static String USERNAME = "username@mail.com";
  private static String BP_CODE = "TOQ-15130";
  private static String CATEGORY_CODE = "CC-123";
  private static String STORE_ID = "10001";
  private static String PICKUP_POINT_CODE = "PP-082";
  private static String BP_NAME = "Toko Qa Test";
  private static String REQUEST_ID = UUID.randomUUID().toString();
  private static String PRODUCT_CODE = "MTA-123";
  private static String BRAND = "Samsung";
  private static double SHIPPING_WEIGHT = 100.0;
  private static String IMG_1 = "img-1.jpg";
  private static String IMG_2 = "img-2.jpg";
  private static String URL_IMAGE =
      "url|https://i.imgur.com/xCx7lQG.png|img-3";
  private static int TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES = 0;

  private static String ATTR_0 = "ATTR-0";
  private static String ATTR_1 = "ATTR-1";
  private static String ATTR_2 = "ATTR-2";
  private static String ATTR_3 = "ATTR-3";
  private static String ATTR_4 = "ATTR-4";
  private static String ATTR_5 = "ATTR-5";
  private static String ATTR_6 = "ATTR-6";
  private static String ATTR_7 = "ATTR-7";

  private static String ATTR_1_VALUE = "ATTR-1-VALUE";

  private static String ATTR_PREDEF_OS = "ATTR-PREDEF-1";
  private static String ATTR_PREDEF_TYPE = "ATTR-PREDEF-2";

  private static final String FAMILY_COLOR_ATTRIBUTE_CODE = "FA-2000033";
  private static final String BLACK_FAMILY_COLOR = "Black";
  private static final String RED_FAMILY_COLOR = "Red";
  private static final double WHOLESALE_DISCOUNT = 20.0;
  private static final int WHOLESALE_QUANTITY = 10;
  private static final String WARNING_MESSAGE = "Warning Message";

  private static final String JPG = "jpg";

  private static final String QUERY_HISTORY_RESULT_VALUE_JSON = "{ productCode: " + PRODUCT_CODE +
      " }";

  private ProductV2Request request;
  private String testingPath = "";
  private Map<String,String> allowedImagesTypes = new HashMap<>();

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(bpRepository);
    Mockito.verifyNoMoreInteractions(categoryRepository);
    Mockito.verifyNoMoreInteractions(generatorRepository);
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(imageValidator);
    Mockito.verifyNoMoreInteractions(trackerService, imageUtil, productAttributeRepository);

    this.removeTmpTestingDir();
  }

  @BeforeEach
  public void initialize() throws Exception{
    MockitoAnnotations.initMocks(this);
    this.buildProductV2Request();

    TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES = (int) request.getProductItems().stream()
        .flatMap(item -> item.getImages().stream()).distinct().count();

    Mockito.when(bpRepository.filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE))
      .thenReturn(this.buildProfileResponse());

    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryResp());

    Mockito.when(categoryRepository.getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any())).thenReturn(this.buildAttrAllowedVal());

    Mockito.when(productRepository.generateProductCode(REQUEST_ID, USERNAME))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));

    Mockito.when(generatorRepository.generateShippingWeight(Mockito.any()))
        .thenReturn(new GenerateShippingWeightResponse(SHIPPING_WEIGHT));

    Mockito.when(objectMapper.writeValueAsString(Mockito.any(CreateProductV2Response.class)))
        .thenReturn(QUERY_HISTORY_RESULT_VALUE_JSON);

    File file = new File(this.getClass().getClassLoader().getResource("CreateProductV2/" + IMG_1).getFile());
    testingPath = file.getParentFile().toString();
    Mockito.when(systemParameter.getMtaImageSource()).thenReturn(file.getParentFile().toString() + File.separator);
    Mockito.when(systemParameter.getMtaApiTmpImage()).thenReturn(file.getParentFile().toString() + File.separator);

    File newTestFile = new File(file.getParentFile().toString() + File.separator + REQUEST_ID + File.separator + IMG_1);
    newTestFile.getParentFile().mkdir();
    Files.copy(file, newTestFile);
    newTestFile = new File(file.getParentFile().toString() + File.separator + REQUEST_ID + File.separator + IMG_2);
    Files.copy(file, newTestFile);

    Mockito.when(productRepository.createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any()))
      .thenReturn(new GdnBaseRestResponse(true));

    BrandResponse brandDetail = new BrandResponse();
    brandDetail.setBrandName(BRAND);
    Mockito.when(categoryRepository.getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND)))
      .thenReturn(brandDetail);

    Mockito.doNothing().when(fileStorageService).moveTmpImageToProductImage(Mockito.eq(REQUEST_ID), Mockito.anySet(), Mockito.eq(PRODUCT_CODE));

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));

    allowedImagesTypes.put("jpg", "ffd8");
    allowedImagesTypes.put("gif", "4749");
    allowedImagesTypes.put("png", "8950");
    allowedImagesTypes.put("webp", "5249");
    ReflectionTestUtils.setField(createProductService, "allowedImageFormatsMap",
        allowedImagesTypes);
  }

  private ProfileResponse buildProfileResponse(){
    PickupPointDTO ppResp = new PickupPointDTO();
    ppResp.setCode(PICKUP_POINT_CODE);
    ProfileResponse profileResp = new ProfileResponse();
    profileResp.setPickupPoints(Collections.singletonList(ppResp));
    CompanyDTO companyResp = new CompanyDTO();
    companyResp.setName(BP_NAME);
    profileResp.setCompany(companyResp);

    return profileResp;
  }

  private List<AllowedAttributeValueDtoResponse> buildAttrAllowedVal() {
    List<AllowedAttributeValueDtoResponse> allowedValueResultList = new ArrayList<>();
    AllowedAttributeValueDtoResponse attrAllowedVal1 = new AllowedAttributeValueDtoResponse();
    attrAllowedVal1.setAttributeCode(ATTR_4);
    attrAllowedVal1.setAllowedValue(Arrays.asList(
        new AllowedValueDtoResponse("123", "WA-03", "Green"),
        new AllowedValueDtoResponse("123", "WA-01", "Black"),
        new AllowedValueDtoResponse("124", "WA-02", "Blue")));
    allowedValueResultList.add(attrAllowedVal1);

    AllowedAttributeValueDtoResponse attrAllowedVal2 = new AllowedAttributeValueDtoResponse();
    attrAllowedVal2.setAttributeCode(ATTR_0);
    attrAllowedVal2.setAllowedValue(Arrays.asList(
        new AllowedValueDtoResponse("321", "BR-05", "Iphone"),
        new AllowedValueDtoResponse("321", "BR-01", BRAND)));
    allowedValueResultList.add(attrAllowedVal2);

    AllowedAttributeValueDtoResponse familyColorAllowedValues = new AllowedAttributeValueDtoResponse();
    familyColorAllowedValues.setAttributeCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    familyColorAllowedValues.setAllowedValue(Arrays.asList(
        new AllowedValueDtoResponse("019", "FA-001", RED_FAMILY_COLOR),
        new AllowedValueDtoResponse("021", "FA-002", BLACK_FAMILY_COLOR)));
    allowedValueResultList.add(familyColorAllowedValues);

    return allowedValueResultList;
  }

  private CategoryDetailResponse buildCategoryNonDefiningAttrResp(){
    CategoryDetailResponse categoryResp = new CategoryDetailResponse();
    categoryResp.setCategoryCode(CATEGORY_CODE);
    categoryResp.setName("category name");
    CatalogResponse catalogResp = new CatalogResponse();
    catalogResp.setId(UUID.randomUUID().toString());
    catalogResp.setStoreId(STORE_ID);
    catalogResp.setName("MASTER_CATALOG");
    catalogResp.setCatalogCode("123");
    catalogResp.setCatalogType("MASTER");
    categoryResp.setCatalog(catalogResp);

    List<CategoryAttributeResponse> catAttrList = new ArrayList<>();
    CategoryAttributeResponse catAttr2 = new CategoryAttributeResponse();
    AttributeResponse attrResp2 = new AttributeResponse();
    attrResp2.setAttributeCode(ATTR_0);
    attrResp2.setName("Brand");
    attrResp2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    catAttr2.setAttribute(attrResp2);
    catAttrList.add(catAttr2);

    CategoryAttributeResponse catAttr3 = new CategoryAttributeResponse();
    AttributeResponse attrResp3 = new AttributeResponse();
    attrResp3.setAttributeCode(ATTR_1);
    attrResp3.setName("Memori");
    attrResp3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    catAttr3.setAttribute(attrResp3);
    catAttrList.add(catAttr3);

    CategoryAttributeResponse catAttr4 = new CategoryAttributeResponse();
    AttributeResponse attrResp4 = new AttributeResponse();
    attrResp4.setAttributeCode(ATTR_3);
    attrResp4.setName("Garansi");
    attrResp4.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp4.setSkuValue(true);
    catAttr4.setAttribute(attrResp4);
    catAttrList.add(catAttr4);

    CategoryAttributeResponse catAttr5 = new CategoryAttributeResponse();
    AttributeResponse attrResp5 = new AttributeResponse();
    attrResp5.setAttributeCode("ABC-123");
    attrResp5.setName("testing");
    attrResp5.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp5.setMarkForDelete(true);
    catAttr5.setAttribute(attrResp5);
    catAttr5.setMarkForDelete(true);
    catAttrList.add(catAttr5);

    categoryResp.setCategoryAttributes(catAttrList);

    return categoryResp;
  }

  private CategoryDetailResponse buildCategoryRespWithoutBrand(){
    CategoryDetailResponse categoryResp = new CategoryDetailResponse();
    categoryResp.setCategoryCode(CATEGORY_CODE);
    categoryResp.setName("category name");
    CatalogResponse catalogResp = new CatalogResponse();
    catalogResp.setId(UUID.randomUUID().toString());
    catalogResp.setStoreId(STORE_ID);
    catalogResp.setName("MASTER_CATALOG");
    catalogResp.setCatalogCode("123");
    catalogResp.setCatalogType("MASTER");
    categoryResp.setCatalog(catalogResp);

    List<CategoryAttributeResponse> catAttrList = new ArrayList<>();
    CategoryAttributeResponse catAttr1 = new CategoryAttributeResponse();
    AttributeResponse attrResp1 = new AttributeResponse();
    attrResp1.setAttributeCode(ATTR_4);
    attrResp1.setName("Warna");
    attrResp1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attrResp1.setBasicView(true);
    attrResp1.setSkuValue(false);
    catAttr1.setAttribute(attrResp1);
    catAttrList.add(catAttr1);

    CategoryAttributeResponse catAttr2 = new CategoryAttributeResponse();
    AttributeResponse attrResp2 = new AttributeResponse();
    attrResp2.setAttributeCode(ATTR_1);
    attrResp2.setName("Memori");
    attrResp2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp2.setBasicView(true);
    attrResp2.setSkuValue(false);
    catAttr2.setAttribute(attrResp2);
    catAttrList.add(catAttr2);

    CategoryAttributeResponse catAttr3 = new CategoryAttributeResponse();
    AttributeResponse attrResp3 = new AttributeResponse();
    attrResp3.setAttributeCode(ATTR_3);
    attrResp3.setName("Garansi");
    attrResp3.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp3.setBasicView(false);
    attrResp3.setSkuValue(false);
    catAttr3.setAttribute(attrResp3);
    catAttrList.add(catAttr3);

    CategoryAttributeResponse catAttr4 = new CategoryAttributeResponse();
    AttributeResponse attrResp4 = new AttributeResponse();
    attrResp4.setAttributeCode("ABC-123");
    attrResp4.setName("testing");
    attrResp4.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp4.setMarkForDelete(true);
    catAttr4.setAttribute(attrResp4);
    catAttr4.setMarkForDelete(true);
    catAttrList.add(catAttr4);

    CategoryAttributeResponse catAttr5 = new CategoryAttributeResponse();
    AttributeResponse attrResp5 = new AttributeResponse();
    attrResp5.setAttributeCode(ATTR_6);
    attrResp5.setName("Jaringan");
    attrResp5.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp5.setBasicView(false);
    attrResp5.setSkuValue(true);
    catAttr5.setAttribute(attrResp5);
    catAttrList.add(catAttr5);

    categoryResp.setCategoryAttributes(catAttrList);
    return categoryResp;
  }

  private CategoryAttributeResponse categoryWithDescriptiveVariantCreatorAttrBuilder(){
    CategoryAttributeResponse catAttrVariantCreator = new CategoryAttributeResponse();
    AttributeResponse attrResp6 = new AttributeResponse();
    attrResp6.setAttributeCode(ATTR_7);
    attrResp6.setName("Variant Creator Attribute");
    attrResp6.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attrResp6.setVariantCreation(true);
    catAttrVariantCreator.setAttribute(attrResp6);
    return catAttrVariantCreator;
  }

  private CategoryDetailResponse buildCategoryResp(){
    CategoryDetailResponse categoryResp = this.buildCategoryRespWithoutBrand();

    CategoryAttributeResponse catAttr1 = new CategoryAttributeResponse();
    AttributeResponse attrResp1 = new AttributeResponse();
    attrResp1.setAttributeCode(ATTR_0);
    attrResp1.setName("Brand");
    attrResp1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    catAttr1.setAttribute(attrResp1);
    categoryResp.getCategoryAttributes().add(catAttr1);
    
    CategoryAttributeResponse familyColorCategoryAttributeResponse =
        new CategoryAttributeResponse();
    AttributeResponse familyColorAttributeResponse = new AttributeResponse();
    familyColorAttributeResponse.setAttributeCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    familyColorAttributeResponse.setName("Family Colour");
    AllowedAttributeValueResponse redFamilyColour = new AllowedAttributeValueResponse();
    redFamilyColour.setValue(RED_FAMILY_COLOR);
    AllowedAttributeValueResponse blackFamilyColor = new AllowedAttributeValueResponse();
    blackFamilyColor.setValue(BLACK_FAMILY_COLOR);
    familyColorAttributeResponse
        .setAllowedAttributeValues(Arrays.asList(redFamilyColour, blackFamilyColor));
    familyColorAttributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    familyColorCategoryAttributeResponse.setAttribute(familyColorAttributeResponse);
    categoryResp.getCategoryAttributes().add(familyColorCategoryAttributeResponse);

    return categoryResp;
  }

  private CategoryDetailResponse buildCategoryWithAdditionalPredefAttr(){
    CategoryDetailResponse categoryResp = this.buildCategoryResp();

    CategoryAttributeResponse catAttr1 = new CategoryAttributeResponse();
    AttributeResponse attrResp1 = new AttributeResponse();
    attrResp1.setAttributeCode(ATTR_PREDEF_OS);
    attrResp1.setName("OS");
    attrResp1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attrResp1.setBasicView(false);
    attrResp1.setSkuValue(false);
    catAttr1.setAttribute(attrResp1);
    categoryResp.getCategoryAttributes().add(catAttr1);

    CategoryAttributeResponse catAttr2 = new CategoryAttributeResponse();
    AttributeResponse attrResp2 = new AttributeResponse();
    attrResp2.setAttributeCode(ATTR_PREDEF_TYPE);
    attrResp2.setName("Hard Case");
    attrResp2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attrResp2.setBasicView(true);
    attrResp2.setSkuValue(true);
    catAttr2.setAttribute(attrResp2);
    categoryResp.getCategoryAttributes().add(catAttr2);

    return categoryResp;
  }

  private CategoryDetailResponse buildCategoryWithPredefSkuValueTrue(){
    CategoryDetailResponse categoryResp = this.buildCategoryResp();

    CategoryAttributeResponse catAttr1 = new CategoryAttributeResponse();
    AttributeResponse attrResp1 = new AttributeResponse();
    attrResp1.setAttributeCode(ATTR_PREDEF_OS);
    attrResp1.setName("OS");
    attrResp1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attrResp1.setBasicView(false);
    attrResp1.setSkuValue(true);
    catAttr1.setAttribute(attrResp1);
    categoryResp.getCategoryAttributes().add(catAttr1);

    return categoryResp;
  }

  /**
   * Build with multiple defining attr, but there's isBasicView false
   * @return
   */
  private CategoryDetailResponse buildCategoryWithMultipleDefining() {
    CategoryDetailResponse categoryResp = this.buildCategoryRespWithoutBrand();

    CategoryAttributeResponse catAttr = new CategoryAttributeResponse();
    AttributeResponse attrResp = new AttributeResponse();
    attrResp.setAttributeCode(ATTR_5);
    attrResp.setName("Ukuran");
    attrResp.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attrResp.setBasicView(false);
    catAttr.setAttribute(attrResp);
    categoryResp.getCategoryAttributes().add(catAttr);

    return categoryResp;
  }

  private void buildProductV2Request() {
    Map<String, String> nonDefAttr = new HashMap<>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");
    nonDefAttr.put(ATTR_7, "Biru Warna Free Text");

    Map<String, List<String>> defAttr = new HashMap<>();
    defAttr.put(ATTR_4, Arrays.asList("Black", "Blue"));

    TreeMap<String, String> attrMap = new TreeMap<>();
    attrMap.put(ATTR_4, "Black");

    TreeMap<String, String> attrMap2 = new TreeMap<String, String>();
    attrMap.put(ATTR_4, "Blue");

    ProductItemV2Request itemReq = new ProductItemV2Request();
    itemReq.setUpcCode("upc");
    itemReq.setMerchantSku("ABC-123");
    itemReq.setPrice(1000.0);
    itemReq.setSalePrice(1000.0);
    itemReq.setStock(1);
    itemReq.setMinimumStock(0);
    itemReq.setDisplayable(false);
    itemReq.setBuyable(false);
    itemReq.setImages(Arrays.asList(IMG_1, IMG_2));
    itemReq.setAttributesMap(attrMap);

    ProductItemV2Request itemReq2 = new ProductItemV2Request();
    itemReq2.setUpcCode("upc");
    itemReq2.setMerchantSku("ABC-123");
    itemReq2.setPrice(1000.0);
    itemReq2.setSalePrice(1000.0);
    itemReq2.setStock(1);
    itemReq2.setMinimumStock(0);
    itemReq2.setDisplayable(false);
    itemReq2.setBuyable(false);
    itemReq2.setImages(Arrays.asList(IMG_1, IMG_2));
    itemReq2.setAttributesMap(attrMap2);
    itemReq2.setWholesalePriceActivated(true);
    ProductWholesaleRequest wholesaleRequest = new ProductWholesaleRequest();
    wholesaleRequest.setDiscount(WHOLESALE_DISCOUNT);
    wholesaleRequest.setQuantity(WHOLESALE_QUANTITY);
    itemReq2.setWholesale(Collections.singletonList(wholesaleRequest));

    request = new ProductV2Request();
    request.setRequestId(REQUEST_ID);
    request.setUsername(USERNAME);
    request.setBpCode(BP_CODE);
    request.setStoreId(STORE_ID);
    request.setName("product testing");
    request.setBrand(BRAND);
    request.setCategoryCode(CATEGORY_CODE);
    request.setProductType(1);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    request.setLength(10.0);
    request.setWidth(10.0);
    request.setHeight(10.0);
    request.setWeight(100.0);
    request.setDescription("desc".getBytes());
    request.setUniqueSellingPoint("desc".getBytes());
    request.setProductStory("desc".getBytes());
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.setProductDefiningAttributes(defAttr);
    request.setProductItems(Arrays.asList(itemReq, itemReq2));
    request.setFamilyColorCode(FAMILY_COLOR_ATTRIBUTE_CODE);
  }

  private List<ProductItemV2Request> productItemsWithVariantCreatorAttributeMapBuilder () {
    TreeMap<String, String> attrMap = new TreeMap<String, String>();
    attrMap.put(ATTR_4, "Black");
    attrMap.put(ATTR_7, "Free text 1");

    ProductItemV2Request itemReq = new ProductItemV2Request();
    itemReq.setUpcCode("upc");
    itemReq.setMerchantSku("ABC-123");
    itemReq.setPrice(1000.0);
    itemReq.setSalePrice(1000.0);
    itemReq.setStock(1);
    itemReq.setMinimumStock(0);
    itemReq.setDisplayable(false);
    itemReq.setBuyable(false);
    itemReq.setImages(Arrays.asList(IMG_1, IMG_2));
    itemReq.setAttributesMap(attrMap);

    ProductItemV2Request itemReq2 = new ProductItemV2Request();
    itemReq2.setUpcCode("upc");
    itemReq2.setMerchantSku("ABC-123");
    itemReq2.setPrice(1000.0);
    itemReq2.setSalePrice(1000.0);
    itemReq2.setStock(1);
    itemReq2.setMinimumStock(0);
    itemReq2.setDisplayable(false);
    itemReq2.setBuyable(false);
    itemReq2.setImages(Arrays.asList(IMG_1, IMG_2));
    itemReq2.setAttributesMap(attrMap);
    ProductWholesaleRequest wholesaleRequest = new ProductWholesaleRequest();
    wholesaleRequest.setDiscount(WHOLESALE_DISCOUNT);
    wholesaleRequest.setQuantity(WHOLESALE_QUANTITY);
    itemReq2.setWholesalePriceActivated(true);
    itemReq2.setWholesale(Collections.singletonList(wholesaleRequest));

    return Arrays.asList(itemReq, itemReq2);
  }

  private void removeTmpTestingDir(){
    File productImage = new File(testingPath + File.separator + PRODUCT_CODE + File.separator + IMG_1);
    FileManager.deleteDirectory(productImage.getParent());
  }

  @Test
  public void process_HappyFlow_Success_DescriptiveVariantCreator_In_DefAttr() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    WarningMessage warningMessage = new WarningMessage();
    warningMessage.setMessage("Warning Message");
    List<WarningMessage> warningMessageList =
        Collections.singletonList(warningMessage);

    WarningMessageDTO warningMessageDTO = new WarningMessageDTO();
    warningMessageDTO.setMessage("Warning Message");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    request.setWarningMessage(warningMessageList);
    when(objectMapper.convertValue(Mockito.eq(warningMessageList), Mockito.any(TypeReference.class)))
        .thenReturn(Collections.singletonList(warningMessageDTO));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(objectMapper).convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class));
    QueueHistoryResultDTO queueHistoryResultDTO = argCaptorQueueHistoryResultDTO.getValue();
    Assertions.assertEquals(1, queueHistoryResultDTO.getWarningMessage().size());
    Assertions.assertEquals("Warning Message", queueHistoryResultDTO.getWarningMessage().get(0).getMessage());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Assertions.assertEquals(productCreationRequestArgumentCaptor.getValue().getProductCreationType(), ProductCreationType.FLOW1_API);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_InvalidFamilyColorValues_Failed() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());
    List<AllowedAttributeValueDtoResponse> allowedAttributeValues = this.buildAttrAllowedVal();
    allowedAttributeValues.stream()
        .filter(allowedAttr -> FAMILY_COLOR_ATTRIBUTE_CODE.equals(allowedAttr.getAttributeCode()))
        .findFirst()
        .ifPresent(allowedAttr -> {
          allowedAttr.setAllowedValue(Arrays.asList(
              new AllowedValueDtoResponse("019", "FA-001", RED_FAMILY_COLOR)));
        });

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    when(categoryRepository.getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
          Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(allowedAttributeValues);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    WarningMessage warningMessage = new WarningMessage();
    warningMessage.setMessage("Warning Message");
    List<WarningMessage> warningMessageList =
        Collections.singletonList(warningMessage);

    WarningMessageDTO warningMessageDTO = new WarningMessageDTO();
    warningMessageDTO.setMessage("Warning Message");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.getProductDefiningAttributes()
        .put(FAMILY_COLOR_ATTRIBUTE_CODE, Arrays.asList(RED_FAMILY_COLOR, BLACK_FAMILY_COLOR));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    request.setWarningMessage(warningMessageList);
    when(objectMapper.convertValue(Mockito.eq(warningMessageList), Mockito.any(TypeReference.class)))
        .thenReturn(Collections.singletonList(warningMessageDTO));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(objectMapper).convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class));
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_AllFamilyColorValuesInvalid_Failed() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());
    List<AllowedAttributeValueDtoResponse> allowedAttributeValues = this.buildAttrAllowedVal();
    allowedAttributeValues.stream()
        .filter(allowedAttr -> FAMILY_COLOR_ATTRIBUTE_CODE.equals(allowedAttr.getAttributeCode()))
        .findFirst()
        .ifPresent(allowedAttr -> allowedAttr.setAllowedValue(null));

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    when(categoryRepository.getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(allowedAttributeValues);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    WarningMessage warningMessage = new WarningMessage();
    warningMessage.setMessage("Warning Message");
    List<WarningMessage> warningMessageList =
        Collections.singletonList(warningMessage);

    WarningMessageDTO warningMessageDTO = new WarningMessageDTO();
    warningMessageDTO.setMessage("Warning Message");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.getProductDefiningAttributes()
        .put(FAMILY_COLOR_ATTRIBUTE_CODE, Arrays.asList(RED_FAMILY_COLOR, BLACK_FAMILY_COLOR));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    request.setWarningMessage(warningMessageList);
    when(objectMapper.convertValue(Mockito.eq(warningMessageList), Mockito.any(TypeReference.class)))
        .thenReturn(Collections.singletonList(warningMessageDTO));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(objectMapper).convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class));
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_HappyFlow_Success_DescriptiveVariantCreator_In_NonDefAttr() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_InvalidBrand_Error() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());

    when(categoryRepository.getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID),
        Mockito.eq(BRAND))).thenReturn(null);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(
        STORE_ID, CATEGORY_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2))
        .validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    QueueHistoryResultDTO queueHistoryResultDTO = argCaptorQueueHistoryResultDTO.getValue();
    Assertions.assertEquals("Can not process invalid input data : Invalid brand value for " +
            BRAND, queueHistoryResultDTO.getErrorMessage());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_HappyFlow_DescriptiveMandatoryAttribute_Doesnt_Exist() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());
    categoryDetailResponse.getCategoryAttributes().get(4).getAttribute().setMandatory(true);

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_HappyFlow_DefiningMandatoryAttribute_Doesnt_Exist() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());
    categoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(true);

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    Map<String, List<String>> defAttr = new HashMap<>();
    defAttr.put(ATTR_7, Arrays.asList("Black", "Blue"));

    TreeMap<String, String> attrMap = new TreeMap<String, String>();
    attrMap.put(ATTR_7, "Free text 1");

    List<ProductItemV2Request> productItems = productItemsWithVariantCreatorAttributeMapBuilder();
    productItems.get(0).setAttributesMap(attrMap);
    productItems.get(1).setAttributesMap(attrMap);

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.setProductDefiningAttributes(defAttr);
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_WithNonDefiningItemAttributes_Success() throws Exception {
    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setFamilyColorCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    List<ProductItemV2Request> productItemV2Requests = request.getProductItems();
    productItemV2Requests.get(0)
        .setNonDefiningItemAttributes(Collections.singletonMap(FAMILY_COLOR_ATTRIBUTE_CODE,
            BLACK_FAMILY_COLOR));
    productItemV2Requests.get(1)
        .setNonDefiningItemAttributes(Collections.singletonMap(FAMILY_COLOR_ATTRIBUTE_CODE,
            RED_FAMILY_COLOR));
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOR_ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);

    when(productAttributeRepository.getAttributeDetailByAttributeCodes(request.getRequestId(),
          request.getUsername(), Collections.singletonList(FAMILY_COLOR_ATTRIBUTE_CODE)))
        .thenReturn(Collections.singletonList(attributeResponse));

    createProductService.process(request);

    ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor =
        ArgumentCaptor.forClass(ProductCreationRequest.class);
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(categoryRepository)
        .getPredefinedAndDefiningAllowedAttributeValue(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any());
    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(imageValidator, times(3)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository)
        .createProduct(Mockito.eq(REQUEST_ID),
            Mockito.eq(USERNAME),
            productCreationRequestArgumentCaptor.capture());

    ProductCreationRequest productCreationRequest = productCreationRequestArgumentCaptor.getValue();
    List<ProductItemCreationRequest> productItemCreationRequests =
        productCreationRequest.getProductItemRequests();
    Assertions.assertEquals(2, productItemCreationRequests.size());
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        productItemCreationRequests.get(0).getProductItemAttributeValueRequests().get(0);
    Assertions.assertEquals(FAMILY_COLOR_ATTRIBUTE_CODE,
        productItemAttributeValueRequest.getAttribute().getAttributeCode());
    Assertions.assertEquals(STORE_ID, productItemAttributeValueRequest.getAttribute().getStoreId());
    Assertions.assertEquals(BLACK_FAMILY_COLOR, productItemAttributeValueRequest.getValue());
    productItemAttributeValueRequest =
        productItemCreationRequests.get(1).getProductItemAttributeValueRequests().get(0);
    Assertions.assertEquals(FAMILY_COLOR_ATTRIBUTE_CODE,
        productItemAttributeValueRequest.getAttribute().getAttributeCode());
    Assertions.assertEquals(STORE_ID, productItemAttributeValueRequest.getAttribute().getStoreId());
    Assertions.assertEquals(RED_FAMILY_COLOR, productItemAttributeValueRequest.getValue());

    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    verify(productAttributeRepository).getAttributeDetailByAttributeCodes(request.getRequestId(),
        request.getUsername(),
        Arrays.asList(FAMILY_COLOR_ATTRIBUTE_CODE));
    verify(systemParameter).getDownloadImageUrlConnectTimeout();
    verify(systemParameter).getDownloadImageUrlReadTimeout();
    verify(imageUtil).validateAndGetImageType(any(), Mockito.anyMap());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_NotFillingNonMandatoryAttr_Success() throws Exception {
    Map<String, String> nonDefAttr = new HashMap<>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "1 year");
    nonDefAttr.put(ATTR_6, "4G");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.setProductNonDefiningAttributes(nonDefAttr);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
          TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_BasicViewTrueAndSkuValueFalse_Success() throws Exception {
    Map<String, String> nonDefAttr = new HashMap<>();
    nonDefAttr.put(ATTR_1, null);

    request.setProductNonDefiningAttributes(nonDefAttr);
    createProductService.process(request);

    ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor =
        ArgumentCaptor.forClass(ProductCreationRequest.class);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_BasicViewFalseAndSkuValueTrue_Success() throws Exception {
    Map<String, String> nonDefAttr = new HashMap<>();
    nonDefAttr.put(ATTR_1, "4 GB");
    nonDefAttr.put(ATTR_2, "1 year");
    nonDefAttr.put(ATTR_6, null);

    request.setProductNonDefiningAttributes(nonDefAttr);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);

    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_EmptyPredefinedWithBasicViewTrue_Success() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryWithAdditionalPredefAttr());

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_EmptyPredefinedWithSkuValueTrue_Success() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryWithPredefSkuValueTrue());

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_BrandAttrNotFound_ThrowException() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryRespWithoutBrand());
    Mockito.when(categoryRepository.getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID),
        Mockito.eq(BRAND))).thenThrow(Exception.class);

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_MultiDefiningWithBasicViewFalse_ThrowException() throws Exception {
    Mockito.when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(this.buildCategoryWithMultipleDefining());

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID,
        CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID),
        Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES))
        .validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }


  @Test
  public void process_WithoutDefiningAndMultipleItem_Error() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryNonDefiningAttrResp());

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_WithoutDefiningAndSingleItem_Error() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
      .thenReturn(this.buildCategoryNonDefiningAttrResp());

    ProductItemV2Request itemReq = new ProductItemV2Request();
    itemReq.setUpcCode("upc");
    itemReq.setMerchantSku("ABC-123");
    itemReq.setPrice(1000.0);
    itemReq.setSalePrice(1000.0);
    itemReq.setStock(1);
    itemReq.setMinimumStock(0);
    itemReq.setDisplayable(false);
    itemReq.setBuyable(false);
    itemReq.setImages(Arrays.asList(IMG_1, IMG_2));
    request.setProductItems(Arrays.asList(itemReq));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
          TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_BopisProduct_Success() throws Exception {
    request.setProductType(3);
    createProductService.process(request);
    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);

    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_FailedWhenMovingTmpImages_Failed() throws Exception {
    Mockito.when(systemParameter.getMtaApiTmpImage()).thenReturn("");
    Mockito.doThrow(ApplicationRuntimeException.class).when(fileStorageService)
        .moveTmpImageToProductImage(Mockito.eq(REQUEST_ID), Mockito.anySet(), Mockito.eq(PRODUCT_CODE));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    File productTmpImage = new File(testingPath + File.separator + REQUEST_ID + File.separator + IMG_1);
    FileManager.deleteDirectory(productTmpImage.getParent());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_EmptyAllowedAttrValueFromPCB_Failed() throws Exception {
    Mockito.when(categoryRepository.getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any())).thenReturn(null);

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_CategoryDetailNotFound_Failed() throws Exception {
    Mockito.when(categoryRepository
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE)).thenReturn(null);

    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES))
        .validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemStockIsNegatif_Failed() throws Exception {
    request.getProductItems().get(0).setStock(-1);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemMinimumStockIsNegatif_Failed() throws Exception {
    request.getProductItems().get(0).setMinimumStock(-1);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemPriceLessThanMinimumPrice_Failed() throws Exception {
    request.getProductItems().get(0).setPrice(10);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemSalePriceGreaterThenRegularPrice_Failed() throws Exception {
    request.getProductItems().get(0).setSalePrice(5000000);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_PickupPointNotValid_Failed() throws Exception {
    request.setPickupPointCode("INVALID-PICKUP-POINT");
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_EmptyNonDefiningAttribute_Failed() throws Exception {
    request.setProductNonDefiningAttributes(new HashMap<>());
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_EmptyDefiningAttribute_Failed() throws Exception {
    request.setProductDefiningAttributes(new HashMap<>());
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemAttrMapIsMoreThanProperAttrList_Failed() throws Exception {
    TreeMap<String, String> attrMap = new TreeMap<>();
    attrMap.put(ATTR_4, "Black");
    attrMap.put("UNKNOWN-ATTR", "-");
    request.getProductItems().get(0).setAttributesMap(attrMap);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_ItemAttrMapIsNotValid_Failed() throws Exception {
    TreeMap<String, String> attrMap = new TreeMap<>();
    attrMap.put("UNKNOWN-ATTR", "-");
    request.getProductItems().get(0).setAttributesMap(attrMap);
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API_V2,
        TrackerConstants.HYPHEN, TrackerConstants.SUCCESS, request.getUsername());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }

  @Test
  public void process_WithWarningMessageFromPBP() throws Exception {
    WarningMessage pbpWarningMsg =
        WarningMessage.builder()
            .message(WARNING_MESSAGE)
            .build();
    WarningMessage originalWarningMsg =
        WarningMessage.builder()
            .message("Original Warning Msg")
            .build();
    List<WarningMessage> originalWarningMsgList = new ArrayList<>();
    originalWarningMsgList.add(originalWarningMsg);
    List<WarningMessage> warningMessageList =
        Arrays.asList(originalWarningMsg, pbpWarningMsg);
    WarningMessageDTO warningMessageDTO = new WarningMessageDTO();
    warningMessageDTO.setMessage("Original Warning Msg");
    WarningMessageDTO warningMessageDTO2 = new WarningMessageDTO();
    warningMessageDTO2.setMessage(WARNING_MESSAGE);
    request.setWarningMessage(originalWarningMsgList);
    Mockito.when(productRepository.createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(WARNING_MESSAGE, null, true, REQUEST_ID));
    Mockito.when(objectMapper.convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class))).thenReturn(Arrays.asList(warningMessageDTO, warningMessageDTO2));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(categoryRepository).getPredefinedAndDefiningAllowedAttributeValue(Mockito.eq(USERNAME),
        Mockito.eq(REQUEST_ID), Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(categoryRepository).getBrandDetail(Mockito.eq(USERNAME), Mockito.eq(REQUEST_ID), Mockito.eq(BRAND));
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(CreateProductV2Response.class));
    Mockito.verify(productRepository).generateProductCode(REQUEST_ID, USERNAME);
    Mockito.verify(productRepository).createProduct(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any());
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any());
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, Mockito.atMost(TOTAL_DISTINCT_PRODUCT_ITEM_IMAGES)).validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
            TrackerConstants.CREATE_FLOW1_API_V2,
            TrackerConstants.HYPHEN,
            TrackerConstants.SUCCESS,
            request.getUsername());
    Mockito.verify(objectMapper).convertValue(Mockito.eq(warningMessageList), Mockito.any(TypeReference.class));
    QueueHistoryResultDTO queueHistoryResultDTO = argCaptorQueueHistoryResultDTO.getValue();
    Assertions.assertEquals(2, queueHistoryResultDTO.getWarningMessage().size());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }
  
  @Test
  public void findDefiningAllowedValue_EmptyList_ReturnNull() throws ApplicationException {
    AttributeResponse attrResp = new AttributeResponse();
    attrResp.setAttributeCode(ATTR_1);
    Map<String, List<AllowedValueDtoResponse>> allowedValueMap = new HashMap<>();
    allowedValueMap.put(ATTR_1, new ArrayList<>());
    AllowedAttributeValueRequest result = 
        CreateProductV2Util.findDefiningAllowedValue(attrResp, null, allowedValueMap);
    Assertions.assertNull(result);
  }

  @Test
  public void findDefiningAllowedValue_valueIsNotValid() {
    AttributeResponse attrResp = new AttributeResponse();
    attrResp.setAttributeCode(ATTR_0);
    Map<String, List<AllowedValueDtoResponse>> allowedValueMap = new HashMap<>();
    allowedValueMap.put(ATTR_1, new ArrayList<>());
    try {
      CreateProductV2Util.findDefiningAllowedValue(attrResp, ATTR_1_VALUE, allowedValueMap);
    } catch (ApplicationException e) {
      Assertions.assertEquals(
          "Can not process invalid input data : The attribute's value is not valid for attribute: "
              + ATTR_0 + " " + "with value: " + ATTR_1_VALUE, e.getErrorMessage());
    }
  }

  @Test
  public void findPredefinedAllowedValue_EmptyList_ReturnNull() throws ApplicationException {
    AttributeResponse attrResp = new AttributeResponse();
    attrResp.setAttributeCode(ATTR_1);
    Map<String, List<AllowedValueDtoResponse>> allowedValueMap = new HashMap<>();
    allowedValueMap.put(ATTR_1, new ArrayList<>());
    PredefinedAllowedAttributeValueRequest result = 
        CreateProductV2Util.findPredefinedAllowedValue(attrResp, null, allowedValueMap);
    Assertions.assertNull(result);
  }

  @Test
  public void findPredefinedAllowedValue_InvalidValue_ThrowException() throws ApplicationException {
    AttributeResponse attrResp = new AttributeResponse();
    attrResp.setAttributeCode(ATTR_1);
    Map<String, List<AllowedValueDtoResponse>> allowedValueMap = new HashMap<>();
    try {
      CreateProductV2Util.findPredefinedAllowedValue(attrResp, ATTR_1_VALUE, allowedValueMap);
    } catch (ApplicationException e) {
      Assertions.assertEquals(e.getErrorMessage(), "Can not process invalid input data : "
          + "The attribute's value is not valid for attribute: " + ATTR_1
          + " with value: " + ATTR_1_VALUE);
    }
  }

  @Test
  public void process_InvalidCategory_QeueuHistoryFalse() throws Exception {
    CategoryDetailResponse categoryDetailResponse= this.buildCategoryResp();
    categoryDetailResponse.getCategoryAttributes()
        .add(categoryWithDescriptiveVariantCreatorAttrBuilder());

    when(systemParameter.getDownloadImageUrlConnectTimeout()).thenReturn(1000);
    when(systemParameter.getDownloadImageUrlReadTimeout()).thenReturn(3000);
    when(imageUtil.validateAndGetImageType(any(), Mockito.eq(allowedImagesTypes))).thenReturn(JPG);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(null);

    Map<String, String> nonDefAttr = new HashMap<String, String>();
    nonDefAttr.put(ATTR_1, "16 GB");
    nonDefAttr.put(ATTR_2, "5 inch");
    nonDefAttr.put(ATTR_3, "1 tahun garansi");
    nonDefAttr.put(ATTR_6, "4G");

    WarningMessage warningMessage = new WarningMessage();
    warningMessage.setMessage("Warning Message");
    List<WarningMessage> warningMessageList =
        Collections.singletonList(warningMessage);

    WarningMessageDTO warningMessageDTO = new WarningMessageDTO();
    warningMessageDTO.setMessage("Warning Message");

    request.setProductStory(null);
    request.setUsername(USERNAME);
    request.getProductItems().get(0).setImages(Arrays.asList(IMG_1, IMG_2, URL_IMAGE));
    request.setProductNonDefiningAttributes(nonDefAttr);
    request.getProductDefiningAttributes()
        .put(ATTR_7, Arrays.asList("Free text 1", "Free text 2"));
    request.setProductItems(productItemsWithVariantCreatorAttributeMapBuilder());
    request.setWarningMessage(warningMessageList);
    when(objectMapper.convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class))).thenReturn(Collections.singletonList(warningMessageDTO));
    createProductService.process(request);

    Mockito.verify(bpRepository).filterByBusinessPartnerCodeV2(STORE_ID, BP_CODE);
    Mockito.verify(categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(STORE_ID, CATEGORY_CODE);
    Mockito.verify(systemParameter, Mockito.atLeastOnce()).getMtaApiTmpImage();
    Mockito.verify(imageValidator, times(2))
        .validateImages(Mockito.any(File.class));
    Mockito.verify(kafkaProducer).send(
        Mockito.eq(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent()),
        argCaptorQueueHistoryResultDTO.capture());
    Mockito.verify(objectMapper).convertValue(Mockito.eq(warningMessageList),
        Mockito.any(TypeReference.class));
    QueueHistoryResultDTO queueHistoryResultDTO = argCaptorQueueHistoryResultDTO.getValue();
    Assertions.assertEquals(1, queueHistoryResultDTO.getWarningMessage().size());
    Assertions.assertEquals("Warning Message", queueHistoryResultDTO.getWarningMessage().get(0).getMessage());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCreateProductV2QueueFeedEvent();
  }
}

