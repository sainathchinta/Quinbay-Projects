package com.gdn.mta.bulk.util;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.ValidateExcelRowsRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.BulkUploadFileHeaders;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductInfoResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

public class BulkCreationCommonUtilTest {

  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String PRODUCT_CODE = "Pcode";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "C3-Id";
  private static final String STORE_ID = "10001";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String REQUEST_ID = "requestId";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String PICKUP_POINT_CODE = "PP-3000785";
  private static final String ATTRIBUTE_VALUE = "Black";
  private static final String Attribute1 = "Att-Id-2";
  private static final String Attribute2 = "Att-Id-3";
  private static final String Category = "C3-Id";
  private static final String BRAND = "brand";
  private static final String NAME = "name";
  private static final String FILE_NAME = "generateInputRowDataDescriptionError";
  private static final String FILE_NAME_2 = "generateInputRowDataDescriptionErrorValidations";
  private static final String IMAGE_URL = "https://i.imgur.com/xCx7lQG.png";
  private static final String IMAGE_URL_INVALID = "https://i.imgur.com/xCx7lQG.pdf";
  private static final String IMAGE_URL_1 = "http://i.imgur.com/xCx7lQG.png";
  private static final String CATEGORY = "Alat Musik";
  private static final String IMAGE_URL_3 = "https://i.imgur.com/xCx7lQG.png";
  private static final String LOCATION = "location";
  private static final String IMAGE_JPG = "image.jpg";
  private static final String BULK_PROCESS_DATA =
      "{\"Kategori*\":\"Alat Musik\",\n\"Kategori1\":\"Aksesoris Alat Musik\",\n\"Kategori2\":\"Strings\",\n\"Nama"
          + "Produk*\":\"Product name\",\n\"Seller SKU\":\"Seller Sku 1\",\n\"Deskripsi*\":\"Description\","
          + "\n\"Keunggulan produk\":\"USP\",\n\"Merek*\":\"ABC\",\n\"Warna*\":\"Black\",\n\"Family Colour\":\"Hitam\","
          + "\n\"Ukuran\":\"\",\n\"Variasi\":\"\",\n\"Parent\":\"1\",\n\"Foto-1*\":\"image-1.jpg\",\n\"Foto-2\":\"\",\n"
          + "\"Foto-3\":\"\",\n\"Fotor-4\":\"\",\n\"Foto-5\":\"\",\n\"Foto-6\":\"\",\n\"Foto-7\":\"\",\n\"Foto-8\":\"\","
          + "\n\"Url Video\":\"URL Video\",\n\"Tipe Penanganan*\":\"Melalui partner logistik Blibli\",\n\"Panjang (cm)*"
          + "\":\"10\",\n\"Lebar (cm)*\":\"10\",\n\"Tinggi (cm)*\":\"10\",\n\"Berat (gram)*\":\"100\",\n\"Harga Penjualan "
          + "(Rp)*\":\"54321\",\n\"Available Stock*\":\"2\",\n\"Pilih Attribut\":\"Fungsi\",\n\"Pilih value\":\"Fungsi value"
          + "\",\n\"Pilih Attribut1\":\"Lain-lain\",\n\"Pilih value1\":\"Lain - lian value\",\n\"Pilih Attribut2\":\"Dimensi"
          + "Produk\",\n\"Pilih value2\":\"Dimensi value\",\n\"Pilih Attribut3\":\"Berat\",\n\"Pilih value3\":\"Berat value"
          + "\",\n\"Pilih Attribut4\":\"Material\",\n\"Pilih value4\":\"material value\"}";
  private static final String VALID_ITEM_SKU = "FBB-60021-00001-00001";
  private static final String INVALID_QUANTITY = "INVALID_QUANTITY";
  private static BulkProcess bulkProcess;
  private static final BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
  private static final BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
  private static final ProductCreationRequest productCreationRequest = new ProductCreationRequest();
  private static final String IMAGE_SOURCE = null;
  private static final int ERROR_COUNT = 102;
  private static final String AUTO_UPLOAD_URL = "aut-upload-url";
  private static final String ATTRIBUTE_CODE_1 = "attributeCode1";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String ATTRIBUTE_CODE_3 = "attributeCode3";
  private static final String ATTRIBUTE_CODE_4 = "attributeCode4";
  private static final String ATTRIBUTE_CODE_5 = "attributeCode5";
  private static final String ATTRIBUTE_CODE_6 = "attributeCode6";
  private static final String ATTRIBUTE_CODE_7 = "attributeCode7";
  private static final String DEFAULT_BLIBLI = "TEC-15624";
  private static final String DEFAULT_PRODUCT_NAME = "DEFAULT_PRODUCT_NAME";
  private static final String FAMILY_COLOUR = "Family Colour";
  private static final String PICKUP_POINT_NAME_DELIMITER = "||";
  private static final String PICKUP_POINT_CODE_AND_NAME = "PP-3000785 || PP-NAME";
  private static final String AUTO_UPLOAD_URL_PREFIX = "auto";
  private static final String AUTO_UPLOAD_URL_IMAGE = "https://example.com/auto/image.jpg";
  private static final String BFB_BASE_PRICE = "bfbBasePrice";
  private static final String BFB_MANAGED = "bfbManaged";
  private static final String BFB_STATUS = "bfbStatus";
  private static final String BFB_BASE_PRICE_VALUE = "1000";
  private static final String BFB_MANAGED_VALUE = "1";
  private static final String BFB_STATUS_VALUE = "1";
  private static final String CM = "CM";
  private static final String SIZE_CHART_DELIMITER = "-";
  private static final List<Integer> eanUpcValidLength = Arrays.asList(5, 8, 12, 13, 14,15);


  private ProductDetailResponse productDetailResponse;
  private ProfileResponse profileResponse;
  private CategoryDetailResponse categoryDetailResponse;
  private Map<String, AttributeResponse> attributeResponseMap;
  private List<ProductSpecialAttributeDTO> productSpecialAttributeDTOList;
  private List<Map<String,String>> cleanDatas;
  private ItemSummaryListResponse itemSummaryListResponse;
  private PriceDTO priceDto;
  private BulkProcessImage bulkProcessImage;
  private Map<String, String> imageUrlMap;
  private CategoryDetailResponse category;
  private String primaryIdentifier = StringUtils.EMPTY;
  private ObjectMapper mapper = new ObjectMapper();
  private ValidateExcelRowsRequest validateExcelRowsRequest;

  @InjectMocks
  private BulkCreationCommonUtil bulkCreationCommonUtil;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setLength(10.0);
    productDetailResponse.setWidth(10.0);
    productDetailResponse.setHeight(10.0);
    productDetailResponse.setWeight(10.0);
    productDetailResponse.setShippingWeight(10.0);
    imageUrlMap = new HashMap<>();

    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse1.setVariantCreation(false);
    attributeResponse1.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse1.setPredefinedAllowedAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeResponse2.setVariantCreation(true);
    attributeResponse2.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse2.setPredefinedAllowedAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE_3);
    attributeResponse3.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponse3.setPredefinedAllowedAttributeValues(new ArrayList<>());
    AttributeResponse attributeResponse5 = new AttributeResponse();
    attributeResponse5.setAttributeCode(ATTRIBUTE_CODE_5);
    attributeResponse5.setName("Brand");
    attributeResponse5.setAttributeType("PREDEFINED_ATTRIBUTE");
    AttributeResponse attributeResponse6 = new AttributeResponse();
    attributeResponse6.setAttributeCode(ATTRIBUTE_CODE_6);
    attributeResponse6.setSkuValue(true);
    AttributeResponse attributeResponse7 = new AttributeResponse();
    attributeResponse7.setAttributeCode(ATTRIBUTE_CODE_7);
    attributeResponse7.setSkuValue(true);
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE_1);
    allowedAttributeValueResponse.setValueType("US");

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("value");
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    productAttributeValueResponse1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse1.setAllowedAttributeValue(allowedAttributeValueResponse);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse2 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse2.setValue("value2");
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse2.setAllowedAttributeValue(allowedAttributeValueResponse);

    attributeResponse3.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse2));
    attributeResponse5.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse1);
    productAttributeResponse1.setProductAttributeValues(Arrays.asList(productAttributeValueResponse2));
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setAttribute(attributeResponse2);
    productAttributeResponse2.setProductAttributeValues(new ArrayList<>());
    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setAttribute(attributeResponse5);
    productAttributeResponse3.setProductAttributeValues(Arrays.asList(productAttributeValueResponse1));

    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse3, productAttributeResponse1, productAttributeResponse2));

    CategoryAttributeResponse categoryAttributeResponse1 = new CategoryAttributeResponse();
    categoryAttributeResponse1.setMarkForDelete(true);
    CategoryAttributeResponse categoryAttributeResponse2 = new CategoryAttributeResponse();
    categoryAttributeResponse2.setAttribute(attributeResponse6);
    CategoryAttributeResponse categoryAttributeResponse3 = new CategoryAttributeResponse();
    categoryAttributeResponse3.setAttribute(attributeResponse1);
    CategoryAttributeResponse categoryAttributeResponse4 = new CategoryAttributeResponse();
    categoryAttributeResponse4.setAttribute(attributeResponse7);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setCompany(new CompanyDTO());

    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName(CATEGORY);
    categoryDetailResponse.setNameEnglish(CATEGORY);
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse1, categoryAttributeResponse2, categoryAttributeResponse3, categoryAttributeResponse4));

    attributeResponseMap = new HashMap<>();
    attributeResponseMap.put(ATTRIBUTE_CODE_5, attributeResponse5);
    attributeResponseMap.put(ATTRIBUTE_CODE_1, attributeResponse1);
    attributeResponseMap.put(ATTRIBUTE_CODE_2, attributeResponse2);
    attributeResponseMap.put(ATTRIBUTE_CODE_3, attributeResponse3);

    ProductSpecialAttributeDTO productSpecialAttributeDTO1 = new ProductSpecialAttributeDTO();
    productSpecialAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE_6);
    productSpecialAttributeDTO1.setAttributeValue("value");
    ProductSpecialAttributeDTO productSpecialAttributeDTO2 = new ProductSpecialAttributeDTO();
    productSpecialAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE_7);
    productSpecialAttributeDTO2.setAttributeValue("");
    productSpecialAttributeDTOList = Arrays.asList(productSpecialAttributeDTO1, productSpecialAttributeDTO2);

    priceDto = new PriceDTO();
    priceDto.setOfferPrice(1000d);

    Set<PriceDTO> priceDTOSet = new HashSet<>();
    priceDTOSet.add(priceDto);

    itemSummaryListResponse =
      ItemSummaryListResponse.builder().itemSku(DEFAULT_BLIBLI).pickupPointCode(PICKUP_POINT_CODE)
        .price(priceDTOSet).masterCategoryCode(CATEGORY).merchantCode(BUSINESS_PARTNER_CODE)
        .brandName(BRAND).productCode(PRODUCT_CODE).build();
    bulkProcessImage = new BulkProcessImage();
    validateExcelRowsRequest = new ValidateExcelRowsRequest();
    validateExcelRowsRequest.setBulkProcessNotes(new BulkProcessNotes());
    validateExcelRowsRequest.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    validateExcelRowsRequest.setInternationalMerchant(true);
    validateExcelRowsRequest.setMinimumPrice(1);
    validateExcelRowsRequest.setMaxStockLimit(1);
    validateExcelRowsRequest.setMerchantStatusType(MerchantStatusType.PURE_DELIVERY);
    validateExcelRowsRequest.setProductBundlingMaxNumberOfSkus(10);
    validateExcelRowsRequest.setProductBundlingEnabled(false);
    validateExcelRowsRequest.setMerchantType("");
    validateExcelRowsRequest.setProductBundlingEligibleMerchantTypes("");
    validateExcelRowsRequest.setCommonImageErrorMessage("");
    validateExcelRowsRequest.setPrimaryIdentifier(primaryIdentifier);

    ReflectionTestUtils.setField(bulkCreationCommonUtil, "allowedImageExtensions",
        List.of("jpg", "jpeg", "png", "webp"));

  }

  private AttributeResponse getBrandAttributeResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericBrandResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private static Map<String, Object> getInputRowData(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    dataMap.put(GenericBulkHeaders.CATEGORY_TREE_HIERARCHY, "Alat Musik->Aksesoris Alat Musik->Strings");
    dataMap.put(GenericBulkHeaders.CN_CATEGORY_ID, "C3-Id");
    dataMap.put(GenericBulkHeaders.CN_CATEGORY_CODE_PARENT_CODE_IDENTIFIER, "1+C3-Id");
    dataMap.put(GenericBulkHeaders.ROW_NUMBER, 10);
    dataMap.put(GenericBulkHeaders.PRODUCT_TYPE, "Bopis");
    return dataMap;
  }

  private static List<Object> getInputRowDataCn(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    List<Object> rowData = new ArrayList<>(dataMap.values());
    return rowData;
  }

  private static Map<String, Object> getInputRowMapDataCn(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    return BulkCreationCommonUtil.removeMandatoryCharactersFromHeaders(dataMap);
  }

  private static List<Object> getInputRowHeadersCn(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    LinkedHashMap<String, Object> dataMap = objectMapper.readValue(inputStream, typeRef);
    List<Object> rowData = new ArrayList<>(dataMap.keySet());
    return rowData;
  }


  private static BulkProcess getBulkProcess() {
    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);
    bulkProcess.setTotalCount(1);
    bulkProcess.setSuccessCount(1);
    bulkProcess.setInternationalMerchant(false);
    bulkProcess.setDescription("general_template (3) copy 3.xlsm. proses validasi berlangsung");
    return bulkProcess;
  }

  private List<CategoryTreeResponse> getGenericCategoryTreeResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryTreePCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<List<CategoryTreeResponse>>() {
    });
  }

  private static BulkProcessData getBulkProcessData() {
    BulkProcessData bulkProcessData =
        BulkProcessData.builder().bulkProcessId(getBulkProcess().getId()).bulkRequestData(BULK_PROCESS_DATA).build();
    return bulkProcessData;
  }

  private static Map<String, CategoryDetailAndShippingResponse> getGenericCategoryDetailResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryDetailPCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    Map<String, CategoryDetailAndShippingResponse> map = new HashMap<>();
    map.put(Category, objectMapper.readValue(inputStream, new TypeReference<CategoryDetailAndShippingResponse>() {
    }));
    return map;
  }

  private static Map<String, String> getImageUrlAndLocationMap() {
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    imageUrlAndLocationMap.put(
        "https://www.static-src.com/ext-catalog/auto-upload-data/TOKOPEDIA_ANN-70070_1_4d1a8bd4-f7ef-401c-9505-717fa7155f55.jpg",
        "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xCx7lQG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xCx7lQ.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xCx7QG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xCxlQG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xC7lQG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/xx7lQG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/Cx7lQG.png", "");
    imageUrlAndLocationMap.put("https://i.imgur.com/Cx7lQG.pdf", "");
    return imageUrlAndLocationMap;
  }

  private static Map<String, List<String>> getAttributesIdAndPossibleValues() {
    Map<String, List<String>> attributesIdAndPossibleValues = new HashMap<>();
    attributesIdAndPossibleValues.put(Attribute1, Arrays
        .asList("Palmerhaus", "Ladfern", "Osberg", "Yunnan Baiyao", "Hua Tuo", "Tze Pao San Pien Pills", "Country",
            "Data Print", "New Hygio", "Bogor Japutra", "ABC"));
    attributesIdAndPossibleValues.put(Attribute2, Arrays
        .asList("Coklat", "Gold", "Silver", "Hijau", "Orange", "Pink", "Kuning", "Merah", "Abu-abu", "Biru", "Hitam",
            "Putih", "Multicolor", "Ungu"));
    return attributesIdAndPossibleValues;
  }

  private static Map<String, List<String>> getcategoryVariantCreationAttributes() {
    Map<String, List<String>> attributesIdAndPossibleValues = new HashMap<>();
    attributesIdAndPossibleValues.put(Attribute1, Arrays.asList("Black","Blue","Grey"));
    attributesIdAndPossibleValues.put(Attribute2, Arrays.asList("1","2","3"));
    return attributesIdAndPossibleValues;
  }

  private ProfileResponse getProfileResponse() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    List<PickupPointDTO> pickupPointList = new ArrayList<PickupPointDTO>();
    PickupPointDTO pickupPoint = new PickupPointDTO();
    pickupPoint.setCode(PICKUP_POINT_CODE);
    pickupPointList.add(pickupPoint);
    profileResponse.setPickupPoints(pickupPointList);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    profileResponse.setCompany(companyDTO);
    return profileResponse;
  }

  private AttributeResponse getDefiningWarnaResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericDefiningWarnaResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  @Test
  public void validateDefiningOrVariantCreationTest_1() throws Exception {
    BulkCreationCommonUtil
        .validateDefiningOrVariantCreation(Map.of("Warna", ""), "", false, "10", getAttributesIdAndPossibleValues().get(Attribute2),
            getDefiningWarnaResponse(), BULK_PROCESS_CODE, bulkUploadErrorCounter, new StringBuilder(), "Warna", true, SIZE_CHART_DELIMITER);
  }

  @Test
  public void validateExcelAttributesTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1).getAttribute().setMarkForDelete(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(3).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setName("Dimensi Produk");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setVariantCreation(false);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setName("Att");
    BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, false, genericCategoryDetailResponse, getAttributesIdAndPossibleValues(),
      false);
  }

  @Test
  public void validateExcelAttributesTest_4() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setName("Dimensi Produk");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setVariantCreation(false);
    Map<String, List<String>> attributeValue = getAttributesIdAndPossibleValues();
    attributeValue.get(Attribute1).set(1, "Dimensi value");
    BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, false, genericCategoryDetailResponse, attributeValue, false);
  }

  @Test
  public void validateExcelAttributesTest_3() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, false, genericCategoryDetailResponse, getAttributesIdAndPossibleValues(),
      false);
    bulkUploadErrorCounter.setFeature(ERROR_COUNT);
    bulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, false, genericCategoryDetailResponse, getAttributesIdAndPossibleValues(),
      false);
  }

  @Test
  public void validateExcelAttributesTest_2() throws Exception {
    Map<String, List<String>> attributeValues = getAttributesIdAndPossibleValues();
    attributeValues.get(Attribute1).set(0, "Dimensi value");
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1).getAttribute().setMarkForDelete(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(3).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setName("Dimensi Produk");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setVariantCreation(false);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setNameEnglish("");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setName("Att");
    BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME_2), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, true, genericCategoryDetailResponse, getAttributesIdAndPossibleValues(),
      false);
  }

  @Test
  public void validateCategory() throws Exception {
    CategoryDetailAndShippingResponse response = getGenericCategoryDetailResponse().get(Category);
    response.setChildCount(1);
    BulkCreationCommonUtil.validateCategory(response, bulkUploadErrorCounter);
  }

  @Test
  public void validateCategory_2() throws Exception {
    CategoryDetailAndShippingResponse response = getGenericCategoryDetailResponse().get(Category);
    response.setMarkForDelete(true);
    BulkCreationCommonUtil.validateCategory(response, bulkUploadErrorCounter);
  }

  @Test
  public void validateCategory_errorCount() throws Exception {
    CategoryDetailAndShippingResponse response = getGenericCategoryDetailResponse().get(Category);
    response.setMarkForDelete(true);
    BulkCreationCommonUtil.validateCategory(response, bulkUploadErrorCounter);
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    BulkCreationCommonUtil.validateCategory(response, bulkUploadErrorCounter);
  }

  @Test
  public void setProductCreationRequestTest() throws Exception {
    productCreationRequest.setBrand(BRAND);
    productCreationRequest.setName(NAME);
    Assertions.assertThrows(RuntimeException.class,
        () -> bulkCreationCommonUtil.setProductCreationRequest(productCreationRequest, bulkProcess,
            PRODUCT_CODE, Arrays.asList(getInputRowData(FILE_NAME_2)), getImageUrlAndLocationMap(),
            IMAGE_SOURCE, getGenericCategoryDetailResponse(), AUTO_UPLOAD_URL, false));
  }

  @Test
  public void setProductCreationRequestTest_2() throws Exception {
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    productCreationRequest.setBrand(BRAND);
    productCreationRequest.setName(NAME);
    Map<String, CategoryDetailAndShippingResponse> categoryMap = getGenericCategoryDetailResponse();
    categoryMap.get(Category).getCategoryAttributes().get(3).setMarkForDelete(true);
    bulkCreationCommonUtil
        .setProductCreationRequest(productCreationRequest, getBulkProcess(), PRODUCT_CODE, Arrays.asList(userRow),
            getImageUrlAndLocationMap(), IMAGE_SOURCE, categoryMap, AUTO_UPLOAD_URL, false);
  }

  @Test
  public void validateEANValueTest() throws Exception {
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    BulkCreationCommonUtil
        .validateEANValue(getInputRowData(FILE_NAME_2), bulkUploadErrorCounter, false, true, new StringBuilder(), "",
            BULK_PROCESS_CODE, new HashSet<>(), eanUpcValidLength);
    BulkCreationCommonUtil
        .validateEANValue(getInputRowData(FILE_NAME), bulkUploadErrorCounter, false, true, new StringBuilder(), "1",
            BULK_PROCESS_CODE, new HashSet<>(), eanUpcValidLength);
  }

  @Test
  public void invalidateEANValueTest() throws Exception {
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    HashSet<String> ean = new HashSet<>();
    ean.add(String.valueOf(12345));
    Map<String, Object> rowData = getInputRowData(FILE_NAME_2);
    rowData.put(GenericBulkHeaders.UPC_EN, 12345);
    BulkCreationCommonUtil
        .validateEANValue(rowData, bulkUploadErrorCounter, false, true, new StringBuilder(), "",
            BULK_PROCESS_CODE, ean, Arrays.asList(1,2));
  }

  @Test
  public void validateDuplicateEANValueTest() throws Exception {
    Map<String, Object> stringObjectMap = getInputRowData(FILE_NAME_2);
    stringObjectMap.put("Model/EAN/UPC", "12345");
    Set<String> upcSet = new HashSet<>();
    upcSet.add("12345");
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    Assertions.assertFalse(
      BulkCreationCommonUtil.validateEANValue(stringObjectMap, bulkUploadErrorCounter, false, true,
        new StringBuilder(), "", BULK_PROCESS_CODE, upcSet, eanUpcValidLength));
    Assertions.assertTrue(
      BulkCreationCommonUtil.validateEANValue(stringObjectMap, bulkUploadErrorCounter, false, true,
        new StringBuilder(), "", BULK_PROCESS_CODE, new HashSet<>(), eanUpcValidLength));
  }

  @Test
  public void validateEANValueInCnTest() throws Exception {
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    Map<String, Object> stringObjectMap = getInputRowData(FILE_NAME_2);
    stringObjectMap.put("Model/EAN/UPC", "12345");
    Set<String> upcSet = new HashSet<>();
    upcSet.add("12345");
    Assertions.assertFalse(
      BulkCreationCommonUtil.validateEANValue(stringObjectMap, bulkUploadErrorCounter, false, true,
        new StringBuilder(), upcSet, Arrays.asList(5, 8, 12, 13)));
    Assertions.assertTrue(
      BulkCreationCommonUtil.validateEANValue(stringObjectMap, bulkUploadErrorCounter, false, true,
        new StringBuilder(), new HashSet<>(), Arrays.asList(5, 8, 12, 13)));
  }

  @Test
  public void validateExcelRowTest() throws Exception {
    bulkUploadErrorCounter.setProductName(ERROR_COUNT);
    bulkUploadErrorCounter.setDescription(ERROR_COUNT);
    bulkUploadErrorCounter.setUniqueSellingPoint(ERROR_COUNT);
    bulkUploadErrorCounter.setProductType(ERROR_COUNT);
    bulkUploadErrorCounter.setHarga(ERROR_COUNT);
    bulkUploadErrorCounter.setHarga(ERROR_COUNT);
    bulkUploadErrorCounter.setHargaPenjualan(ERROR_COUNT);
    bulkUploadErrorCounter.setStock(ERROR_COUNT);
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    bulkUploadErrorCounter.setHeight(ERROR_COUNT);
    bulkUploadErrorCounter.setWeight(ERROR_COUNT);
    bulkUploadErrorCounter.setWidth(ERROR_COUNT);
    bulkUploadErrorCounter.setLength(ERROR_COUNT);
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    BulkCreationCommonUtil.validateExcelRow(getInputRowData("generateInputRowDataLongDesc"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "", false,
        false, false, validateExcelRowsRequest);
    Map<String, Object> excelRowMap = getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template");
    BulkCreationCommonUtil.validateExcelRow(excelRowMap, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "", false, false, false, validateExcelRowsRequest);
    excelRowMap.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
    BulkCreationCommonUtil.validateExcelRow(excelRowMap, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "", false, false, false, validateExcelRowsRequest);
    BulkCreationCommonUtil.validateExcelRow(
        getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "", false,
        false, false, validateExcelRowsRequest);
    BulkCreationCommonUtil.validateExcelRow(
        getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.BFB, 10, false, "", "", "", false, false, false,
        validateExcelRowsRequest);
    BulkCreationCommonUtil.validateExcelRow(
        getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.BFB, 10, false, "", "", "", true, false, false,
        validateExcelRowsRequest);
    Map<String, Object> rowData = getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2");
    rowData.put(GenericBulkHeaders.PRODUCT_TYPE, "Dikirimkan oleh seller");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, false, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.INSTORE, "");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.INSTORE, NAME);
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.INSTORE, "3");
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
            MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest));
    rowData.put(GenericBulkHeaders.LENGTH, "");
    rowData.put(GenericBulkHeaders.HEIGTH, "");
    rowData.put(GenericBulkHeaders.WIDTH, "");
    rowData.put(GenericBulkHeaders.WEIGHT, "");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, NAME);
    rowData.put(GenericBulkHeaders.HEIGTH, NAME);
    rowData.put(GenericBulkHeaders.WIDTH, NAME);
    rowData.put(GenericBulkHeaders.WEIGHT, NAME);
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, "0");
    rowData.put(GenericBulkHeaders.HEIGTH, "0");
    rowData.put(GenericBulkHeaders.WIDTH, "0");
    rowData.put(GenericBulkHeaders.WEIGHT, "0");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, "0");
    rowData.put(GenericBulkHeaders.HEIGTH, "0");
    rowData.put(GenericBulkHeaders.WIDTH, "0");
    rowData.put(GenericBulkHeaders.WEIGHT, "0");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, true, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, "-1");
    rowData.put(GenericBulkHeaders.HEIGTH, "-1");
    rowData.put(GenericBulkHeaders.WIDTH, "-1");
    rowData.put(GenericBulkHeaders.WEIGHT, "-1");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, true, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.DESCRIPTION, "");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, false, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, true, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH_EN, "-1");
    rowData.put(GenericBulkHeaders.HEIGTH_EN, "-1");
    rowData.put(GenericBulkHeaders.WIDTH_EN, "-1");
    rowData.put(GenericBulkHeaders.WEIGHT_EN, "-1");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, true, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, "0");
    rowData.put(GenericBulkHeaders.HEIGTH, "0");
    rowData.put(GenericBulkHeaders.WIDTH, "0");
    rowData.put(GenericBulkHeaders.WEIGHT, "0");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, true, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, false, true, validateExcelRowsRequest);
    rowData.put(GenericBulkHeaders.LENGTH, "");
    rowData.put(GenericBulkHeaders.HEIGTH, "");
    rowData.put(GenericBulkHeaders.WIDTH, "");
    rowData.put(GenericBulkHeaders.WEIGHT, "");
    BulkCreationCommonUtil.validateExcelRow(rowData, bulkProcessNotes, bulkUploadErrorCounter, true, 1, 10000000L,
        MerchantStatusType.BFB, 10, false, "", "", "", true, true, true, validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowTest_2() throws Exception {
    bulkUploadErrorCounter.setFeature(ERROR_COUNT);
    BulkCreationCommonUtil.validateExcelRow(getInputRowData("generateInputRowDataLongDesc"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "",
        false, false, false, validateExcelRowsRequest);
    BulkCreationCommonUtil.validateExcelRow(
        getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "",
        false, false, false, validateExcelRowsRequest);
    BulkCreationCommonUtil.validateExcelRow(
        getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, true, "TD", "TD", "",
        false, false, false, validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowTest_3() throws Exception {
    bulkUploadErrorCounter.setHeight(ERROR_COUNT);
    bulkUploadErrorCounter.setWidth(ERROR_COUNT);
    bulkUploadErrorCounter.setLength(ERROR_COUNT);
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    BulkCreationCommonUtil.validateExcelRow(getInputRowData("generateInputRowDataDescriptionError2"), bulkProcessNotes,
        bulkUploadErrorCounter, false, 1, 10000000L, MerchantStatusType.PURE_DELIVERY, 10, false, "", "", "",
        false, false, false, validateExcelRowsRequest);
  }

  @Test
  public void generateProductItemNamesTest() throws Exception {
    BulkCreationCommonUtil
        .generateProductItemNamesForGenericCreation(Arrays.asList(getInputRowData(FILE_NAME)), getcategoryVariantCreationAttributes());
  }

  @Test
  public void validateDefiningOrVariantCreationTest() throws Exception {
    AttributeResponse attributeResponse = getDefiningWarnaResponse();
    attributeResponse.setMandatory(false);
    BulkCreationCommonUtil
        .validateDefiningOrVariantCreation(Map.of("Warna", ""), "", false, "10", getAttributesIdAndPossibleValues().get(Attribute2),
            attributeResponse, BULK_PROCESS_CODE, bulkUploadErrorCounter, new StringBuilder(), "Warna", true, SIZE_CHART_DELIMITER);
    bulkUploadErrorCounter.setVariation(ERROR_COUNT);
    BulkCreationCommonUtil.validateDefiningOrVariantCreation(Map.of("Warna", "ATTRIBUTE_VALUE"), ATTRIBUTE_VALUE, false, "10",
        getAttributesIdAndPossibleValues().get(Attribute2), getDefiningWarnaResponse(), BULK_PROCESS_CODE,
        bulkUploadErrorCounter, new StringBuilder(), "Warna", true, SIZE_CHART_DELIMITER);
  }

  @Test
  public void getUserInputCategoryTreeAndChildCategoryTest() throws Exception {
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.CN, "");
    BulkCreationCommonUtil.getUserInputCategoryTreeAndChildCategory(Arrays.asList(userRow), new HashSet<>());
    userRow.put(GenericBulkHeaders.CATEGORY, "");
    BulkCreationCommonUtil.getUserInputCategoryTreeAndChildCategory(Arrays.asList(userRow), new HashSet<>());
  }

  @Test
  public void getMatchingCategoryHierarchyDelimitedAndCategoryIdMapTest() throws Exception {
    Map<String, String> categoryHierarchy = new HashMap<>();
    categoryHierarchy.put(CATEGORY, "Strings");
    Set<String> userInputC1CategoryNames = new HashSet<>();
    userInputC1CategoryNames.add(CATEGORY);
    List<CategoryTreeResponse> categoryTreeResponses = getGenericCategoryTreeResponse();
    categoryTreeResponses.forEach(categoryTreeResponse -> categoryTreeResponse.setChildren(new ArrayList<>()));
    BulkCreationCommonUtil
        .getMatchingCategoryHierarchyDelimitedAndCategoryIdMap(categoryHierarchy, userInputC1CategoryNames,
            categoryTreeResponses, false);
    Assertions.assertEquals(categoryHierarchy.get(CATEGORY), "Test-Parent-00001");
  }

  @Test
  public void validateExcelPickupPointsTest() throws Exception {
    Map<String, PickupPointResponse> pickupPointDTOMap = new HashMap<>();
    pickupPointDTOMap.put("PP-3000782", new PickupPointResponse());
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    Map<String, Object> raw = getInputRowData(FILE_NAME);
    String pickupPointHeader = GenericBulkHeaders.PICKUP_POINT.replace("*", "");
    raw.put(pickupPointHeader , "PP-3000782 || PP-NAME");
    BulkCreationCommonUtil.validateExcelPickupPoints(raw, pickupPointDTOMap, bulkProcessNotes,
        bulkUploadErrorCounter, false, "||", null);
  }

  @Test
  public void validateExcelPickupPoints_inaccessiblePickupPointTest() throws Exception {
    Map<String, PickupPointResponse> pickupPointDTOMap = new HashMap<>();
    pickupPointDTOMap.put("PP-3000782", new PickupPointResponse());
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    Map<String, Object> raw = getInputRowData(FILE_NAME);
    String pickupPointHeader = GenericBulkHeaders.PICKUP_POINT.replace("*", "");
    raw.put(pickupPointHeader , "PP-3000782 || PP-NAME");
    Assertions.assertFalse(BulkCreationCommonUtil.validateExcelPickupPoints(raw, pickupPointDTOMap, bulkProcessNotes,
        bulkUploadErrorCounter, false, "||", Collections.singleton("PP-3000781")));
  }

  @Test
  public void validateExcelPickupPoints_accessiblePickupPointTest() throws Exception {
    Map<String, PickupPointResponse> pickupPointDTOMap = new HashMap<>();
    pickupPointDTOMap.put("PP-3000782", new PickupPointResponse());
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    Map<String, Object> raw = getInputRowData(FILE_NAME);
    String pickupPointHeader = GenericBulkHeaders.PICKUP_POINT.replace("*", "");
    raw.put(pickupPointHeader , "PP-3000782 || PP-NAME");
    Assertions.assertTrue(BulkCreationCommonUtil.validateExcelPickupPoints(raw, pickupPointDTOMap, bulkProcessNotes,
        bulkUploadErrorCounter, false, "||", Collections.singleton("PP-3000782")));
  }

  @Test
  public void validateExcelPickupPointsNotContainsTest() throws Exception {
    Map<String, PickupPointResponse> pickupPointDTOMap = new HashMap<>();
    pickupPointDTOMap.put("PP-3000782", new PickupPointResponse());
    bulkUploadErrorCounter.setPickupPoint(ERROR_COUNT);
    Map<String, Object> raw = getInputRowData(FILE_NAME);
    BulkCreationCommonUtil.validateExcelPickupPoints(raw, pickupPointDTOMap, bulkProcessNotes,
        bulkUploadErrorCounter, false, "||", null);
  }

  @Test
  public void validateExcelImagesForSingleRowTest() throws Exception {
    bulkUploadErrorCounter.setImage(ERROR_COUNT);
    Map<String, Object> row = getInputRowData(FILE_NAME);
    row.put(GenericBulkHeaders.IMAGE_1, "");
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), new ArrayList<>(), 1);
    bulkUploadErrorCounter.setImage(1);
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), new ArrayList<>(), 1);
  }

  @Test
  public void validateExcelImagesForSingleRowTest_NonUrl() throws Exception {
    Map<String, Object> row = getInputRowData(FILE_NAME);
    row.put(GenericBulkHeaders.IMAGE_1, "image-1.jpg");
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), new ArrayList<>(), 1);
    bulkUploadErrorCounter.setImage(1);
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), new ArrayList<>(), 1);
  }

  @Test
  public void validateExcelImagesForSingleRowTest_ErrorCount() throws Exception {
    Map<String, Object> row = getInputRowData(FILE_NAME);
    row.put(GenericBulkHeaders.IMAGE_1, IMAGE_URL);
    row.put(GenericBulkHeaders.IMAGE_2, IMAGE_URL_1);
    row.put(GenericBulkHeaders.IMAGE_3, IMAGE_URL_INVALID);
    bulkUploadErrorCounter.setImage(ERROR_COUNT);
    List<BulkProcessImage> invalidUrl = new ArrayList<>();
    BulkProcessImage bulkProcessImage =
        BulkProcessImage.builder().imageURL(IMAGE_URL_INVALID).errorMessage("msg").build();
    invalidUrl.add(bulkProcessImage);
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), invalidUrl, 1);
    bulkUploadErrorCounter.setImage(0);
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), invalidUrl, 1);
    row.put(GenericBulkHeaders.IMAGE_1, IMAGE_URL_INVALID);
    bulkCreationCommonUtil
        .validateExcelImagesForSingleRow(Arrays.asList(row), getBulkProcess(), bulkUploadErrorCounter, false,
            new HashMap<>(), invalidUrl, 1);
  }

  @Test
  public void getCategoryVariantCreationAttributesTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> categoryMap = getGenericCategoryDetailResponse();
    categoryMap.get(Category).getCategoryAttributes()
        .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    categoryMap.get(Category).getCategoryAttributes().get(0).getAttribute().setName("varioiweh");
    categoryMap.get(Category).getCategoryAttributes().get(4).getAttribute().setName("variasi");
    categoryMap.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    BulkCreationCommonUtil.getCategoryVariantCreationAttributes(categoryMap, new HashMap<>(), new HashMap<>(), false);
  }

  @Test
  public void convertUserRowsToProductCollectionRequests() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse
        .setPredefinedAllowedAttributeValues(getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setId("2");
    attributeResponse1.setName("Dimensi def");
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse1, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.GENERATED_ITEM_NAME, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);

    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);

    userRow.put(GenericBulkHeaders.SALE_PRICE, "111");
    userRow.put(GenericBulkHeaders.STOCK, "abc");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
  }

  @Test
  public void convertUserRowsToProductCollectionRequestsForBopisTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse
        .setPredefinedAllowedAttributeValues(getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setId("2");
    attributeResponse1.setName("Dimensi def");
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse1, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.GENERATED_ITEM_NAME, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    userRow.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "111");
    userRow.put(GenericBulkHeaders.STOCK, "1");
    ProductCreationRequest productCreationRequest = BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
    Assertions.assertEquals(0.0, productCreationRequest.getHeight(), 0);
    Assertions.assertEquals(0.0, productCreationRequest.getWeight(), 0);
    Assertions.assertEquals(0.0, productCreationRequest.getWidth(), 0);
    Assertions.assertEquals(0.0, productCreationRequest.getLength(), 0);
  }

  @Test
  public void isWarnaAndDescriptiveTest() {
    BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.PREDEFINED_ATTRIBUTE.name()).build(), Constant.COLOR);
  }

  @Test
  public void setStateAndFetchInvalidRows() throws IOException {
    BulkCreationCommonUtil
        .setStateAndFetchInvalidRows(getBulkProcess(), Arrays.asList(getBulkProcessData(), getBulkProcessData()));
    BulkProcessData bulkProcessData = getBulkProcessData();
    bulkProcessData.setInputErrorCount(0);
    bulkProcessData.setSystemErrorCount(0);
    BulkCreationCommonUtil
        .setStateAndFetchInvalidRows(getBulkProcess(), Arrays.asList(bulkProcessData, bulkProcessData));
    BulkProcess bulkProcess = getBulkProcess();
    bulkProcess.setSuccessCount(0);
    BulkCreationCommonUtil.setStateAndFetchInvalidRows(bulkProcess, Arrays.asList(bulkProcessData, bulkProcessData));
  }

  @Test
  public void isDescriptiveVariantTest() throws IOException {
    AttributeResponse attribute = new AttributeResponse();
    attribute.setVariantCreation(true);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    Assertions.assertTrue(BulkCreationCommonUtil.isDescriptiveVariant(attribute));
  }

  @Test
  public void isDescriptiveVariantTest1() throws IOException {
    AttributeResponse attribute = new AttributeResponse();
    attribute.setVariantCreation(false);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    Assertions.assertFalse(BulkCreationCommonUtil.isDescriptiveVariant(attribute));
  }

  @Test
  public void isDescriptiveVariantTest2() throws IOException {
    AttributeResponse attribute = new AttributeResponse();
    attribute.setVariantCreation(false);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    Assertions.assertFalse(BulkCreationCommonUtil.isDescriptiveVariant(attribute));
  }

  @Test
  public void isDescriptiveVariantTest3() throws IOException {
    AttributeResponse attribute = new AttributeResponse();
    attribute.setVariantCreation(true);
    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    Assertions.assertFalse(BulkCreationCommonUtil.isDescriptiveVariant(attribute));
  }

  @Test
  public void checkIfEmptyOrNotApplicableTest() throws IOException {
    Assertions.assertFalse(BulkCreationCommonUtil.checkIfEmptyOrNotApplicable(""));
  }

  @Test
  public void checkIfEmptyOrNotApplicableTest1() throws IOException {
    Assertions.assertFalse(BulkCreationCommonUtil.checkIfEmptyOrNotApplicable(GenericBulkParameters.NOT_APPLICABLE));
  }

  @Test
  public void checkIfEmptyOrNotApplicableTest2() throws IOException {
    Assertions.assertTrue(BulkCreationCommonUtil.checkIfEmptyOrNotApplicable("ABCD"));
  }

  @Test
  public void isWarnaAndDescriptive1Test() {
    Assertions.assertFalse(BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.PREDEFINED_ATTRIBUTE.name()).build(), Constant.COLOR));
  }

  @Test
  public void isWarnaAndDescriptive2Test() {
    Assertions.assertTrue(BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name()).build(), Constant.COLOR));
  }

  @Test
  public void isWarnaAndDescriptive3Test() {
    Assertions.assertTrue(BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name()).build(), Constant.WARNA));
  }

  @Test
  public void isWarnaAndDescriptive4Test() {
    Assertions.assertFalse(BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name()).build(),
        Constant.BRAND_IN_REVIEW));
  }

  @Test
  public void isWarnaAndDescriptive5Test() {
    Assertions.assertFalse(BulkCreationCommonUtil.isWarnaAndDescriptive(
        AttributeResponse.builder().attributeType(AttributeType.PREDEFINED_ATTRIBUTE.name()).build(),
        Constant.BRAND_IN_REVIEW));
  }


  @Test
  public void prepareProductRequestTestInternationalMerchantFalse() {
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.prepareProductRequest(productDetailResponse, profileResponse, categoryDetailResponse);
    Assertions.assertEquals(10.0, productCreationRequest.getLength().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getHeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWidth().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(CATEGORY, productCreationRequest.getCategoryName());
  }

  @Test
  public void prepareProductRequestTestInternationalMerchantFalseAndEnglishNameNull() {
    categoryDetailResponse.setNameEnglish(null);
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.prepareProductRequest(productDetailResponse, profileResponse, categoryDetailResponse);
    Assertions.assertEquals(10.0, productCreationRequest.getLength().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getHeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWidth().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(CATEGORY, productCreationRequest.getCategoryName());
  }

  @Test
  public void prepareProductRequestTestInternationalMerchantTrueAndEnglishNameNull() {
    profileResponse.getCompany().setInternationalFlag(true);
    categoryDetailResponse.setNameEnglish(null);
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.prepareProductRequest(productDetailResponse, profileResponse, categoryDetailResponse);
    Assertions.assertEquals(10.0, productCreationRequest.getLength().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getHeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWidth().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(CATEGORY, productCreationRequest.getCategoryName());
  }

  @Test
  public void prepareProductRequestTestInternationalMerchantTrue() {
    profileResponse.getCompany().setInternationalFlag(true);
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.prepareProductRequest(productDetailResponse, profileResponse, categoryDetailResponse);
    Assertions.assertEquals(10.0, productCreationRequest.getLength().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getHeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getWidth().doubleValue(), 0);
    Assertions.assertEquals(10.0, productCreationRequest.getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(CATEGORY, productCreationRequest.getCategoryName());
  }

  @Test
  public void initializeNonVariantCreationAttributesTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    BulkCreationCommonUtil.initializeNonVariantCreationAttributes(attributeResponseMap, true, productCreationRequest,
        existingAttributes);
    Assertions.assertNotNull(productCreationRequest.getProductAttributes());
    Assertions.assertEquals("value", productCreationRequest.getBrand());
  }

  @Test
  public void initializeNonVariantCreationAttributesEmptyAttributeTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      attributeResponse.setMandatory(false);
      if (BRAND.equalsIgnoreCase(attributeResponse.getName())) {
        continue;
      }
      if (CollectionUtils.isNotEmpty(productAttributeResponse.getProductAttributeValues())) {
        productAttributeResponse.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(null);
        productAttributeResponse.getProductAttributeValues().get(0).setDescriptiveAttributeValue(null);
        productAttributeResponse.getProductAttributeValues().get(0).setAllowedAttributeValue(null);
      }
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    BulkCreationCommonUtil.initializeNonVariantCreationAttributes(attributeResponseMap, true, productCreationRequest,
        existingAttributes);
    Assertions.assertNotNull(productCreationRequest.getProductAttributes());
  }

  @Test
  public void initializeNonVariantCreationMandatoryAttributesEmptyAttributeTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      attributeResponse.setMandatory(true);
      attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
      if (CollectionUtils.isNotEmpty(productAttributeResponse.getProductAttributeValues())) {
        productAttributeResponse.getProductAttributeValues().get(0).setDescriptiveAttributeValue(null);
        productAttributeResponse.getProductAttributeValues().get(0).setPredefinedAllowedAttributeValue(null);
        productAttributeResponse.getProductAttributeValues().get(0).setAllowedAttributeValue(null);
      }
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    Assertions.assertThrows(Exception.class,
        () -> BulkCreationCommonUtil.initializeNonVariantCreationAttributes(attributeResponseMap,
            true, productCreationRequest, existingAttributes));
  }

  @Test
  public void initializeNonVariantCreationAttributesExceptionTest() throws Exception {
    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE_4);
    attributeResponse4.setMandatory(true);
    attributeResponse4.setAttributeType("PREDEFINED_ATTRIBUTE");
    attributeResponseMap.put(ATTRIBUTE_CODE_4, attributeResponse4);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    Assertions.assertThrows(Exception.class,
        () -> BulkCreationCommonUtil.initializeNonVariantCreationAttributes(attributeResponseMap,
            true, productCreationRequest, existingAttributes));
  }

  @Test
  public void generateProductBusinessPartnerRequestTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    BulkCreationCommonUtil.generateProductBusinessPartnerRequest(productSpecialAttributeDTOList, categoryDetailResponse, productCreationRequest);
    Assertions.assertNotNull(productCreationRequest.getProductBusinessPartnerAttributes());
    Assertions.assertEquals(2, productCreationRequest.getProductBusinessPartnerAttributes().size());
  }

  @Test
  public void prepareProtectedBrandNameCodeMapTest(){
    ProtectedBrandResponse protectedBrandResponse = new ProtectedBrandResponse();
    protectedBrandResponse.setBrandCode(BRAND);
    protectedBrandResponse.setBrandName(NAME);
    Map<String, String> result =
      bulkCreationCommonUtil.prepareProtectedBrandNameCodeMap(Arrays.asList(protectedBrandResponse));
    Assertions.assertEquals(result.size(),1);
  }

  @Test
  public void validateDeliveryStatusCncFalseTest() throws Exception {
    Map<String, Object> row = getInputRowData("generateInputRowDataMPPSwitchOnCncFalse");
    row.put(GenericBulkHeaders.SALE_PRICE, "1");

    bulkCreationCommonUtil.setItemPickupPointDetails(row, row, MerchantStatusType.DELIVERY_AND_CNC, "||",
        false, false);
  }

  @Test
  public void validateDeliveryStatusCncFalsePickupPointNameConcatTest() throws Exception {
    Map<String, Object> row = getInputRowData("generateInputRowDataMPPSwitchOnCncFalse");
    row.put(GenericBulkHeaders.SALE_PRICE, "1");
    row.put(GenericBulkHeaders.PICKUP_POINT, "PP-3000785 || PP-NAME");
    PickupPointCreateRequest pickupPointCreateRequest =
        bulkCreationCommonUtil.setItemPickupPointDetails(row, row, MerchantStatusType.DELIVERY_AND_CNC, "||",
            false, false);
    Assertions.assertEquals("PP-3000785", pickupPointCreateRequest.getPickupPointId());
  }

  @Test
  public void validateDeliveryStatusCncFalse1Test() throws Exception {
    Map<String, Object> row = getInputRowData("generateInputRowDataMPPSwitchOnCncFalse");
    row.put(GenericBulkHeaders.SALE_PRICE, "1");


    bulkCreationCommonUtil.setItemPickupPointDetails(row, row, MerchantStatusType.PURE_DELIVERY, "||",
        false, false);
  }

  @Test
  public void validateDeliveryStatusExceptionTest() throws Exception {
    bulkCreationCommonUtil.setItemPickupPointDetails(getInputRowData("InvalidDataInput2MPPCnc"),
        getInputRowData("generateInputRowDataMPPSwitchOnCnc"), MerchantStatusType.DELIVERY_AND_CNC, "||",
        false, false);
  }

  @Test
  @Disabled
  public void setItemPickupPointTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 9);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 6);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 18);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 28);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 24);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.PURE_DELIVERY,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }

  @Test
  public void setItemPickupPointCncTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 10);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 7);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 30);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 19);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 25);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCnc_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCnc_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    currentRow.put(BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER, "PP-3000785 || PP-NAME");
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.DELIVERY_AND_CNC,
            "||", true, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }

  @Test
  public void setItemPickupPointBFBCncTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 10);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 7);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 30);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 19);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 25);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCnc_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCnc_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    currentRow.put(BulkCnCreationHeaderNames.BFB_STATUS_HEADER, "");
    currentRow.put(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, "");
    currentRow.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "");
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.BFB_AND_CNC,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }

  @Test
  public void setItemPickupPointCncOffTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 10);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 7);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 30);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 19);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 25);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.DELIVERY_AND_CNC,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }

  @Test
  public void setItemPickupPointCncOnTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 10);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 7);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 30);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 19);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 25);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    currentRow.put(BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME, 1);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME, 1);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.DELIVERY_AND_CNC,
            "||", true, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
    Assertions.assertEquals(result.isCncBuyable(), false);
    Assertions.assertEquals(result.isCncDisplay(), true);
    Assertions.assertEquals(result.isBuyable(), false);
    Assertions.assertEquals(result.isDisplay(), true);
  }
  @Test
  public void setItemPickupPointInvalidDataTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 9);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 6);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 18);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 28);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 24);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnInvalidData_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.PURE_DELIVERY,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }

  @Test
  public void setItemPickupPointBlankDataTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 9);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 6);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 18);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 28);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 24);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnBlankData_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.PURE_DELIVERY,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
  }


  @Test
  public void setItemPickupPointInvalidItemNameTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 9);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 6);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 18);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 28);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 24);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, "IPhone256g");
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
        bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.PURE_DELIVERY,
            "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), null);
  }

  @Test
  public void toItemSkuAndPPCodeRequestTest(){
  cleanDatas = generateExcelData(1);

    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
    BulkCreationCommonUtil.toItemSkuAndPPCodeRequest(cleanDatas);
    Assertions.assertEquals(result.size(),1);
  }

  @Test
  public void toItemListDtoListTest(){
    cleanDatas = generateExcelData(1);
    List<ItemDetailsDto> itemDetailsDtoList = BulkCreationCommonUtil.toItemListDtoList(cleanDatas);
    Assertions.assertEquals(itemDetailsDtoList.get(0).getItemSku(),DEFAULT_BLIBLI);
    Assertions.assertEquals(itemDetailsDtoList.get(0).getPickUpPointCode(),"PP-3000297");

  }

  @Test
  public void toProductLevel3SummaryResponseTest(){
    ProductLevel3SummaryResponse response =
      BulkCreationCommonUtil.toProductLevel3SummaryResponse(itemSummaryListResponse);
    Assertions.assertEquals(response.getBrand(),BRAND);
  }

  @Test
  public void toAddDetailsForCampaignV2WithValidMerchantPromoDiscountTest(){
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    discountPriceDTO.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPriceDTO.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));

    Set<PriceDTO> priceDTOSet = new HashSet<>();
    priceDto.setMerchantPromoDiscountPrice(discountPriceDTO);
    priceDTOSet.add(priceDto);
    itemSummaryListResponse.setPrice(priceDTOSet);
    itemSummaryListResponse.setMerchantPromoDiscount(true);
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
      itemSkuPPCodeSummaryMap.put(DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE,
        itemSummaryListResponse);
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap,cleanDatas);
    Assertions.assertEquals(result.size(),1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void toAddDetailsForCampaignV2WithInValidMerchantPromoDiscountTest() {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    discountPriceDTO.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPriceDTO.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(2))));

    Set<PriceDTO> priceDTOSet = new HashSet<>();
    priceDto.setMerchantPromoDiscountPrice(discountPriceDTO);
    priceDTOSet.add(priceDto);
    itemSummaryListResponse.setPrice(priceDTOSet);
    itemSummaryListResponse.setMerchantPromoDiscount(true);
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
    itemSkuPPCodeSummaryMap.put(DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE, itemSummaryListResponse);
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap, cleanDatas);
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void toAddDetailsForCampaignV2MerchantPromoTrueNullMerchantDiscountTest() {
    itemSummaryListResponse.setMerchantPromoDiscount(true);
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
    itemSkuPPCodeSummaryMap.put(DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE, itemSummaryListResponse);
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap, cleanDatas);
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void toAddDetailsForCampaignV2() {
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
    itemSkuPPCodeSummaryMap.put(DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE, itemSummaryListResponse);
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap, cleanDatas);
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void toAddDetailsForCampaignV2WithInValidMerchantPromoDiscount_2Test() {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    discountPriceDTO.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPriceDTO.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(2))));

    Set<PriceDTO> priceDTOSet = new HashSet<>();
    priceDto.setMerchantPromoDiscountPrice(discountPriceDTO);
    priceDTOSet.add(priceDto);
    itemSummaryListResponse.setPrice(priceDTOSet);
    itemSummaryListResponse.setMerchantPromoDiscount(true);
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
    itemSkuPPCodeSummaryMap.put(DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE, itemSummaryListResponse);
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap, cleanDatas);
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void toAddDetailsForCampaignNullV2Test(){
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap = new HashMap<>();
    cleanDatas = generateExcelData(1);
    List<Map<String, String>> result =
      BulkCreationCommonUtil.toAddDetailsForCampaignV2(itemSkuPPCodeSummaryMap,cleanDatas);
    Assertions.assertEquals(result.size(),1);
    Assertions.assertNotNull(result);
  }

  private List<Map<String,String>> generateExcelData(int size){
    List<Map<String,String>> excelData = new ArrayList<>();
    for(int i =0 ; i < size; i++){
      Map<String,String> productRow = new HashMap<>();
      productRow.put(BulkParameters.BLIBLI_SKU, DEFAULT_BLIBLI);
      productRow.put(BulkParameters.PRODUCT_NAME, DEFAULT_PRODUCT_NAME + i);
      productRow.put(BulkParameters.SKU_CODE_HEADER, "001" + 1);
      productRow.put(BulkParameters.SELLER_SKU, "9092");
      productRow.put(BulkParameters.PRICE_HEADER, "3000");
      productRow.put(BulkParameters.SELLING_PRICE_HEADER, "2500");
      productRow.put(BulkParameters.STOCK_HEADER, "100");
      productRow.put(BulkParameters.TYPE_HANDLING_HEADER, "1");
      productRow.put(BulkParameters.PICKUP_POINT_HEADER, "PP-3000297");
      productRow.put(BulkParameters.AMPHI_SKU_STATUS, "0");
      productRow.put(BulkParameters.EXTERNAL_SKU_STATUS, "1.0");
      productRow.put(BulkParameters.PICKUP_POINT_CODE, "PP-3000297");
      productRow.put(BulkParameters.ITEM_SKU_PP_CODE, DEFAULT_BLIBLI + "-" + PICKUP_POINT_CODE);
      excelData.add(productRow);
    }
    return excelData;
  }

  @Test
  public void getProductCreationBulkProcessTypeForPrioritySellersTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkCreationCommonUtil.getProductCreationBulkProcessTypeForPrioritySellers(false, profileResponse));
    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkCreationCommonUtil.getProductCreationBulkProcessTypeForPrioritySellers(true, profileResponse));

    profileResponse.setTrustedSeller(true);
    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
        BulkCreationCommonUtil.getProductCreationBulkProcessTypeForPrioritySellers(true, profileResponse));
  }

  @Test
  public void getBulkProcessTypeBasedOnTrustedSellerAndUserInputRowsTest() {
    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(), 1, 10000, 10, 100, false));

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(), 9999, 10000, 10, 100, true));

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(), 10001, 10000, 10, 100, true));

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(), 9, 10000, 10, 100, true));

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(), 99, 10000, 10, 100, true));

    Assertions.assertEquals(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
            BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(), 101, 10000, 10, 100, true));
  }

  @Test
  public void getImageDownloadStatusByPriorityTest() {
    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_PROCESSING,
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
            false));

    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1,
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
            true));

    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2,
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue(),
            true));

    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_PROCESSING,
        BulkCreationCommonUtil.getImageDownloadStatusByPriority(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
            true));
  }

  @Test
  public void getBasicInfoImageDownloadStatusByPriorityTest() {
    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1,
        BulkCreationCommonUtil.getBasicInfoImageDownloadStatusByPriority(
            BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue()));
    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2,
        BulkCreationCommonUtil.getBasicInfoImageDownloadStatusByPriority(
            BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue()));
    Assertions.assertEquals(BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING,
        BulkCreationCommonUtil.getBasicInfoImageDownloadStatusByPriority(
            BulkProcessType.PRODUCT_BASIC_INFO.getValue()));
  }

  @Test
  public void getBulkProcessTypesForCreationTest() {
    Assertions.assertEquals(ImmutableSet.of(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
            BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
            BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
            BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue()),
        BulkCreationCommonUtil.getBulkProcessTypesForCreation(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue()));

    Assertions.assertEquals(ImmutableSet.of(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
            BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue()),
        BulkCreationCommonUtil.getBulkProcessTypesForCreation(BulkProcessType.PRODUCT_LEVEL_3.getValue()));

    Assertions.assertEquals(ImmutableSet.of(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue()),
        BulkCreationCommonUtil.getBulkProcessTypesForCreation(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue()));

    Assertions.assertEquals(ImmutableSet.of(BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue()),
      BulkCreationCommonUtil.getBulkProcessTypesForCreation(
        BulkProcessType.PRODUCT_BASIC_INFO.getValue()));

  }

  @Test
  public void setItemPickupPointDefaultValuesTest() throws Exception {
    Map<String, Integer> indexColumnGroup = new HashMap<>();
    indexColumnGroup.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroup.put(BulkUploadFileHeaders.IMAGES, 10);
    indexColumnGroup.put(BulkUploadFileHeaders.VARIANT, 7);
    indexColumnGroup.put(BulkUploadFileHeaders.ATTRIBUTES, 30);
    indexColumnGroup.put(BulkUploadFileHeaders.SHIPMENT_INFO, 19);
    indexColumnGroup.put(BulkUploadFileHeaders.SKU_VISIBILITY, 29);
    indexColumnGroup.put(BulkUploadFileHeaders.PRICE_AND_STOCK, 25);
    String itemGeneratedName = "IPhone256gb";
    Map<String, Object> currentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_CnDefault");
    Map<String, Object> parentRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOnCncOff_CnDefault");
    currentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    currentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    parentRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, 10);
    parentRow.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, itemGeneratedName);
    PickupPointCreateRequest result =
      bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0), MerchantStatusType.DELIVERY_AND_CNC,
          "||", false, false);
    Assertions.assertEquals(result.getPickupPointId(), "PP-3000785");
    bulkCreationCommonUtil.setItemPickupPoint(itemGeneratedName, currentRow, parentRow, new AtomicInteger(0),
        MerchantStatusType.DELIVERY_AND_CNC, "||", false, true);
  }

  @Test
  public void validateDeliveryStatusCncFalseDefaultTest() throws Exception {
    Map<String, Object> row = getInputRowData("generateInputRowDataMPPSwitchOnCncFalseDefault");
    row.put(GenericBulkHeaders.SALE_PRICE, "1");

    bulkCreationCommonUtil.setItemPickupPointDetails(row, row, MerchantStatusType.DELIVERY_AND_CNC, "||",
        true, false);
  }

  @Test
  public void getPriorityTest() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    Assertions.assertEquals(1, BulkCreationCommonUtil.getPriority(bulkProcess));

    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    Assertions.assertEquals(2, BulkCreationCommonUtil.getPriority(bulkProcess));

    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue());
    Assertions.assertEquals(0, BulkCreationCommonUtil.getPriority(bulkProcess));
  }

  @Test
  public void validateFamilyColourTest() {
    Map<String, Object> row = ImmutableMap.of(FAMILY_COLOUR, FAMILY_COLOUR);
    Map<String, List<String>> attributeValueMap = ImmutableMap.of(FAMILY_COLOUR, Arrays.asList(FAMILY_COLOUR));
    Assertions.assertTrue(BulkCreationCommonUtil.validateFamilyColour(FAMILY_COLOUR, FAMILY_COLOUR, row, attributeValueMap));

    attributeValueMap = ImmutableMap.of(FAMILY_COLOUR, Arrays.asList(ATTRIBUTE_VALUE));
    Assertions.assertFalse(BulkCreationCommonUtil.validateFamilyColour(FAMILY_COLOUR, FAMILY_COLOUR, row, attributeValueMap));

    row = ImmutableMap.of(FAMILY_COLOUR, FAMILY_COLOUR);
    Assertions.assertFalse(BulkCreationCommonUtil.validateFamilyColour(FAMILY_COLOUR, FAMILY_COLOUR, row, attributeValueMap));

  }

  @Test
  public void getValueBasedOnEnOrIdHeaderTest() {
    Map<String, Object> row = ImmutableMap.of(Constant.BRAND, BRAND, BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    Assertions.assertEquals(BRAND, BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, Constant.BRAND, Constant.BRAND));
    Assertions.assertEquals(BRAND,
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, Constant.BRAND_IN_REVIEW, Constant.BRAND));
    Assertions.assertEquals(1, BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, BulkCnCreationHeaderNames.ROW_NUMBER,
        BulkCnCreationHeaderNames.ROW_NUMBER));
  }

  @Test
  public void getAttributeNameBasedOnHeaderTest() {
    Assertions.assertEquals(BRAND, BulkCreationCommonUtil.getAttributeNameBasedOnHeader(BRAND, BRAND, false));
    Assertions.assertEquals(BRAND, BulkCreationCommonUtil.getAttributeNameBasedOnHeader(BRAND, BRAND, true));
    Assertions.assertEquals(BRAND, BulkCreationCommonUtil.getAttributeNameBasedOnHeader("", BRAND, true));
  }

  @Test
  public void generateProductBusinessPartnerRequestForCategoryUploadTest() {
    Map<String, Object> row = ImmutableMap.of(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setName(ATTRIBUTE_CODE_1);
    attributeResponse.setSkuValue(true);
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));
    BulkCreationCommonUtil.generateProductBusinessPartnerRequestForCategoryUpload(Arrays.asList(row),
        categoryDetailResponse, new ProductCreationRequest(), new BulkProcess(), false);
  }

  @Test
  public void validateExcelAttributesCreationTest() throws Exception {
    boolean isInternationMerchant = true;
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setMarkForDelete(true);

    AttributeResponse specialAttributeResponse1 = new AttributeResponse();
    specialAttributeResponse1.setName(ATTRIBUTE_CODE_1);
    specialAttributeResponse1.setSkuValue(true);
    specialAttributeResponse1.setMandatory(true);

    AttributeResponse specialAttributeResponse2 = new AttributeResponse();
    specialAttributeResponse2.setName(ATTRIBUTE_CODE_2);
    specialAttributeResponse2.setSkuValue(true);
    specialAttributeResponse2.setMandatory(false);

    AttributeResponse specialAttributeResponse3 = new AttributeResponse();
    specialAttributeResponse3.setName(ATTRIBUTE_CODE_3);
    specialAttributeResponse3.setSkuValue(true);
    specialAttributeResponse3.setMandatory(false);

    AttributeResponse predefinedAttribute1 = new AttributeResponse();
    predefinedAttribute1.setNameEnglish(ATTRIBUTE_CODE_3);
    predefinedAttribute1.setName(ATTRIBUTE_CODE_3);
    predefinedAttribute1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute1.setMandatory(false);
    AttributeResponse predefinedAttribute2 = new AttributeResponse();
    predefinedAttribute2.setNameEnglish(ATTRIBUTE_CODE_4);
    predefinedAttribute2.setName(ATTRIBUTE_CODE_4);
    predefinedAttribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute2.setMandatory(true);
    AttributeResponse predefinedAttribute3 = new AttributeResponse();
    predefinedAttribute3.setNameEnglish(ATTRIBUTE_CODE_5);
    predefinedAttribute3.setName(ATTRIBUTE_CODE_5);
    predefinedAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute3.setMandatory(true);
    AttributeResponse predefinedAttribute4 = new AttributeResponse();
    predefinedAttribute4.setNameEnglish(ATTRIBUTE_CODE_6);
    predefinedAttribute4.setName(ATTRIBUTE_CODE_6);
    predefinedAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute4.setMandatory(false);

    AttributeResponse brandAttribute = new AttributeResponse();
    brandAttribute.setName(Constant.BRAND);

    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_4, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_5, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_6, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_1, "");
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);


    Map<String, List<String>> attributesValues = new HashMap<>();
    attributesValues.put(ATTRIBUTE_CODE_4, Arrays.asList(ATTRIBUTE_VALUE));
    attributesValues.put(ATTRIBUTE_CODE_5, Arrays.asList(ATTRIBUTE_VALUE));
    attributesValues.put(ATTRIBUTE_CODE_6, new ArrayList<>());
    attributesValues.put(Constant.BRAND, new ArrayList<>());

    CategoryAttributeResponse categoryAttributeResponse1 = new CategoryAttributeResponse();
    categoryAttributeResponse1.setAttribute(attributeResponse1);

    CategoryAttributeResponse categoryAttributeResponse2 = new CategoryAttributeResponse();
    categoryAttributeResponse2.setAttribute(specialAttributeResponse1);
    CategoryAttributeResponse categoryAttributeResponse3 = new CategoryAttributeResponse();
    categoryAttributeResponse3.setAttribute(specialAttributeResponse2);
    CategoryAttributeResponse categoryAttributeResponse4 = new CategoryAttributeResponse();
    categoryAttributeResponse4.setAttribute(specialAttributeResponse3);

    CategoryAttributeResponse categoryAttributeResponse5 = new CategoryAttributeResponse();
    categoryAttributeResponse5.setAttribute(predefinedAttribute1);
    CategoryAttributeResponse categoryAttributeResponse6 = new CategoryAttributeResponse();
    categoryAttributeResponse6.setAttribute(predefinedAttribute2);
    CategoryAttributeResponse categoryAttributeResponse7 = new CategoryAttributeResponse();
    categoryAttributeResponse7.setAttribute(predefinedAttribute3);
    CategoryAttributeResponse categoryAttributeResponse8 = new CategoryAttributeResponse();
    categoryAttributeResponse8.setAttribute(predefinedAttribute4);

    CategoryAttributeResponse categoryAttributeResponse9 = new CategoryAttributeResponse();
    categoryAttributeResponse9.setAttribute(brandAttribute);


    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(
        Arrays.asList(categoryAttributeResponse1, categoryAttributeResponse2, categoryAttributeResponse3,
            categoryAttributeResponse4, categoryAttributeResponse5, categoryAttributeResponse6,
            categoryAttributeResponse7, categoryAttributeResponse8, categoryAttributeResponse9));

    BulkCreationCommonUtil.validateExcelAttributes(row, attributesValues, categoryDetailResponse, new BulkProcess(),
        new BulkProcessNotes(), new BulkUploadErrorCounter(), isInternationMerchant, false);
  }

  @Test
  public void validateExcelAttributesCreationFetchIgnoreBrandTest() throws Exception {
    boolean isInternationMerchant = true;
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setMarkForDelete(true);

    AttributeResponse specialAttributeResponse1 = new AttributeResponse();
    specialAttributeResponse1.setName(ATTRIBUTE_CODE_1);
    specialAttributeResponse1.setSkuValue(true);
    specialAttributeResponse1.setMandatory(true);

    AttributeResponse specialAttributeResponse2 = new AttributeResponse();
    specialAttributeResponse2.setName(ATTRIBUTE_CODE_2);
    specialAttributeResponse2.setSkuValue(true);
    specialAttributeResponse2.setMandatory(false);

    AttributeResponse specialAttributeResponse3 = new AttributeResponse();
    specialAttributeResponse3.setName(ATTRIBUTE_CODE_3);
    specialAttributeResponse3.setSkuValue(true);
    specialAttributeResponse3.setMandatory(false);

    AttributeResponse predefinedAttribute1 = new AttributeResponse();
    predefinedAttribute1.setNameEnglish(ATTRIBUTE_CODE_3);
    predefinedAttribute1.setName(ATTRIBUTE_CODE_3);
    predefinedAttribute1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute1.setMandatory(false);
    AttributeResponse predefinedAttribute2 = new AttributeResponse();
    predefinedAttribute2.setNameEnglish(ATTRIBUTE_CODE_4);
    predefinedAttribute2.setName(ATTRIBUTE_CODE_4);
    predefinedAttribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute2.setMandatory(true);
    AttributeResponse predefinedAttribute3 = new AttributeResponse();
    predefinedAttribute3.setNameEnglish(ATTRIBUTE_CODE_5);
    predefinedAttribute3.setName(ATTRIBUTE_CODE_5);
    predefinedAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute3.setMandatory(true);
    AttributeResponse predefinedAttribute4 = new AttributeResponse();
    predefinedAttribute4.setNameEnglish(ATTRIBUTE_CODE_6);
    predefinedAttribute4.setName(ATTRIBUTE_CODE_6);
    predefinedAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    predefinedAttribute4.setMandatory(false);

    AttributeResponse brandAttribute = new AttributeResponse();
    brandAttribute.setName(Constant.BRAND);

    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_4, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_5, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_6, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE);
    row.put(ATTRIBUTE_CODE_1, "");
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);


    Map<String, List<String>> attributesValues = new HashMap<>();
    attributesValues.put(ATTRIBUTE_CODE_4, Arrays.asList(ATTRIBUTE_VALUE));
    attributesValues.put(ATTRIBUTE_CODE_5, Arrays.asList(ATTRIBUTE_VALUE));
    attributesValues.put(ATTRIBUTE_CODE_6, new ArrayList<>());
    attributesValues.put(Constant.BRAND, new ArrayList<>());

    CategoryAttributeResponse categoryAttributeResponse1 = new CategoryAttributeResponse();
    categoryAttributeResponse1.setAttribute(attributeResponse1);

    CategoryAttributeResponse categoryAttributeResponse2 = new CategoryAttributeResponse();
    categoryAttributeResponse2.setAttribute(specialAttributeResponse1);
    CategoryAttributeResponse categoryAttributeResponse3 = new CategoryAttributeResponse();
    categoryAttributeResponse3.setAttribute(specialAttributeResponse2);
    CategoryAttributeResponse categoryAttributeResponse4 = new CategoryAttributeResponse();
    categoryAttributeResponse4.setAttribute(specialAttributeResponse3);

    CategoryAttributeResponse categoryAttributeResponse5 = new CategoryAttributeResponse();
    categoryAttributeResponse5.setAttribute(predefinedAttribute1);
    CategoryAttributeResponse categoryAttributeResponse6 = new CategoryAttributeResponse();
    categoryAttributeResponse6.setAttribute(predefinedAttribute2);
    CategoryAttributeResponse categoryAttributeResponse7 = new CategoryAttributeResponse();
    categoryAttributeResponse7.setAttribute(predefinedAttribute3);
    CategoryAttributeResponse categoryAttributeResponse8 = new CategoryAttributeResponse();
    categoryAttributeResponse8.setAttribute(predefinedAttribute4);

    CategoryAttributeResponse categoryAttributeResponse9 = new CategoryAttributeResponse();
    categoryAttributeResponse9.setAttribute(brandAttribute);


    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(
      Arrays.asList(categoryAttributeResponse1, categoryAttributeResponse2, categoryAttributeResponse3,
        categoryAttributeResponse4, categoryAttributeResponse5, categoryAttributeResponse6,
        categoryAttributeResponse7, categoryAttributeResponse8, categoryAttributeResponse9));

    BulkCreationCommonUtil.validateExcelAttributes(row, attributesValues, categoryDetailResponse, new BulkProcess(),
      new BulkProcessNotes(), new BulkUploadErrorCounter(), isInternationMerchant, true);
  }

  @Test
  public void validateExcelRowsEmptyTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.LENGTH_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.HEIGHT_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.WIDTH_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME, "");
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    validateExcelRowsRequest.setRaw(row);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    validateExcelRowsRequest.setPureInstoreProduct(true);
    row.put(BulkCnCreationHeaderNames.LENGTH_EN_HEADER_NAME, "");
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    row.put(BulkCnCreationHeaderNames.WIDTH_EN_HEADER_NAME, "");
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    row.put(BulkCnCreationHeaderNames.HEIGHT_ID_HEADER_NAME, "");
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "");
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowsDecimalSaleTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME, "1.9999");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "1.9999");
    validateExcelRowsRequest.setRaw(row);
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(true);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowsWeightTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    validateExcelRowsRequest.setRaw(row);
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(true);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowsWeightAndProductTypeTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    validateExcelRowsRequest.setRaw(row);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    row.put(BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_ID_HEADER_NAME, "");
    validateExcelRowsRequest.setPureInstoreProduct(true);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    validateExcelRowsRequest.setRaw(row);
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(true);
    validateExcelRowsRequest.setSellerBopisEligible(true);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void validateExcelRowsWeightAndProductTypeInstoreTest() throws JsonProcessingException {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER, "1234567890123");
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    validateExcelRowsRequest.setRaw(row);
    validateExcelRowsRequest.setInternationalMerchant(false);
    row.put(BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME, "");
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
    row.put(GenericBulkHeaders.INSTORE, "");
    validateExcelRowsRequest.setInstoreSeller(true);
    row.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(true));
    validateExcelRowsRequest.setPrimaryIdentifier(mapper.writeValueAsString(row));
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(true);
    validateExcelRowsRequest.setSellerBopisEligible(true);
    validateExcelRowsRequest.setRaw(row);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void overrideEmptyProductTypeTest() throws JsonProcessingException {
    validateExcelRowsRequest.setSellerBopisEligible(true);
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER, "1234567890123");
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    validateExcelRowsRequest.setRaw(row);
    row.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(false));
    validateExcelRowsRequest.setPrimaryIdentifier(mapper.writeValueAsString(row));
    validateExcelRowsRequest.setBopisCategoryValidationForSellerTypes(CM);
    validateExcelRowsRequest.setMerchantType(PRODUCT_CODE);
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(true);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
    row.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(true));
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
    validateExcelRowsRequest.setMerchantType(CM);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
    validateExcelRowsRequest.setCategoryBopisEligible(false);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
    validateExcelRowsRequest.setCategoryBopisEligible(true);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, StringUtils.EMPTY);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, StringUtils.EMPTY);
    validateExcelRowsRequest.setMerchantType(PRODUCT_CODE);
    BulkCreationCommonUtil.overrideEmptyProductType(validateExcelRowsRequest, row, mapper.writeValueAsString(row),
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);
  }

  @Test
  public void validateExcelRowsWeightAndProductTypeDescriptionTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    row.put(BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME, StringUtils.EMPTY);
    validateExcelRowsRequest.setRaw(row);
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest));
  }

  @Test
  public void validateExcelRowsResultTrueTest() throws Exception {
    Map<String, Object> row = getInputRowData(FILE_NAME);
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.PRODUCT_NAME_EN_HEADER_NAME, DEFAULT_PRODUCT_NAME);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    row.put(BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME, PRODUCT_NAME);
    row.put(BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME, "0");
    validateExcelRowsRequest.setRaw(row);
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest));
  }

  @Test
  public void validateExcelRowsWeightAndProductTypeBundlingSwitchOnTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME, "-1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    validateExcelRowsRequest.setProductBundlingEnabled(true);
    validateExcelRowsRequest.setMerchantType("TD");
    validateExcelRowsRequest.setProductBundlingEligibleMerchantTypes("TD");
    validateExcelRowsRequest.setRaw(row);
    BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
  }

  @Test
  public void validateGroupRowAttributesTest() {
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setMandatory(true);
    attributeResponse1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse1.setName(ATTRIBUTE_CODE_1);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setMandatory(true);
    attributeResponse2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse2.setName(ATTRIBUTE_CODE_2);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setName(ATTRIBUTE_CODE_3);
    attributeResponse3.setMandatory(true);
    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setName(ATTRIBUTE_CODE_4);
    attributeResponse4.setMandatory(false);
    Map<String, AttributeResponse> attributeResponseMap = new HashMap<>();
    attributeResponseMap.put(ATTRIBUTE_CODE_1, attributeResponse1);
    attributeResponseMap.put(ATTRIBUTE_CODE_2, attributeResponse2);
    attributeResponseMap.put(ATTRIBUTE_CODE_3, attributeResponse3);
    attributeResponseMap.put(ATTRIBUTE_CODE_4, attributeResponse4);

    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_1, "-");
    row.put(ATTRIBUTE_CODE_2, "value");
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);

    BulkCreationCommonUtil.validateGroupRowAttributes(row, attributeResponseMap, new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true);
  }

  @Test
  public void validateFamilyColourAttributeTest() {
    Map<String, List<String>> attributeValues = new HashMap<>();
    attributeValues.put(Constant.COLOUR_FAMILY, Arrays.asList(FAMILY_COLOUR));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setName(Constant.COLOUR_FAMILY);
    attributeResponse.setNameEnglish(Constant.COLOUR_FAMILY);

    Map<String, Object> row = new HashMap<>();
    row.put(Constant.COLOUR_FAMILY, FAMILY_COLOUR);
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);

    BulkCreationCommonUtil.validateFamilyColourAttribute(row, Constant.COLOUR_FAMILY, attributeResponse,
        attributeValues, true);

    attributeValues.put(Constant.COLOUR_FAMILY, new ArrayList<>());
    BulkCreationCommonUtil.validateFamilyColourAttribute(row, Constant.COLOUR_FAMILY, attributeResponse,
        attributeValues, true);

    BulkCreationCommonUtil.validateFamilyColourAttribute(row, Constant.COLOUR_FAMILY, attributeResponse,
        attributeValues, false);
  }

  @Test
  public void isAttributeNotFamilyColorNorEanUPCTest() {
    Assertions.assertTrue(
        BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(true, ATTRIBUTE_CODE_1, new AttributeResponse()));
    Assertions.assertFalse(BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(true, ATTRIBUTE_CODE_1, null));
    Assertions.assertFalse(BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(true, Constant.EAN_UPC, null));
    Assertions.assertFalse(BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(true, Constant.COLOUR_FAMILY, null));
    Assertions.assertFalse(BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(false, Constant.COLOUR_FAMILY, null));
  }

  @Test
  public void validatePredefinedAttributeValuesTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setName(ATTRIBUTE_CODE_1);
    List<String> attributeValues = Arrays.asList(ATTRIBUTE_VALUE);

    Assertions.assertFalse(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, attributeValues, SIZE_CHART_DELIMITER));

    Assertions.assertTrue(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, new ArrayList<>(), SIZE_CHART_DELIMITER));

    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_2);
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, new ArrayList<>(), SIZE_CHART_DELIMITER));

    attributeResponse.setMandatory(true);
    row.put(ATTRIBUTE_CODE_1, "");
    Assertions.assertTrue(BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, new ArrayList<>(),
        SIZE_CHART_DELIMITER));


    attributeResponse.setMandatory(true);
    row.put(ATTRIBUTE_CODE_1, "S");
    Assertions.assertFalse(BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, Arrays.asList("UK-S"),
        SIZE_CHART_DELIMITER));

    attributeResponse.setName("");
    attributeResponse.setNameEnglish(ATTRIBUTE_CODE_1);
    attributeResponse.setMandatory(true);
    row.put(ATTRIBUTE_CODE_1, "S");
    Assertions.assertFalse(BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, Arrays.asList("UK-S"),
        SIZE_CHART_DELIMITER));

    attributeResponse.setMandatory(true);
    row.put(ATTRIBUTE_CODE_1, "S");
    Assertions.assertTrue(BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, Arrays.asList("UK-S", "US-S"),
        SIZE_CHART_DELIMITER));
  }

  @Test
  public void validateMandatoryPreDefinedAttributeTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setName(ATTRIBUTE_CODE_1);
    attributeResponse.setMandatory(true);
    List<String> attributeValues = Arrays.asList(ATTRIBUTE_VALUE);

    Assertions.assertFalse(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, attributeValues, SIZE_CHART_DELIMITER));

    Assertions.assertTrue(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, new ArrayList<>(), SIZE_CHART_DELIMITER));

    attributeResponse.setMandatory(false);
    row.put(ATTRIBUTE_CODE_1, "");
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateDefiningAttributeValues(row, attributeResponse, new ArrayList<>(), SIZE_CHART_DELIMITER));
  }

  @Test
  public void getMerchantTypeTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constant.B2B_SELLER_CHANNEL));
    companyDTO.setCncActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);

    Assertions.assertEquals(MerchantStatusType.BFB_AND_CNC, BulkCreationCommonUtil.getMerchantType(profileResponse));

    companyDTO.setCncActivated(false);
    Assertions.assertEquals(MerchantStatusType.BFB, BulkCreationCommonUtil.getMerchantType(profileResponse));

    companyDTO.setSalesChannel(new ArrayList<>());
    companyDTO.setCncActivated(true);
    Assertions.assertEquals(MerchantStatusType.DELIVERY_AND_CNC, BulkCreationCommonUtil.getMerchantType(profileResponse));

    companyDTO.setSalesChannel(new ArrayList<>());
    companyDTO.setCncActivated(false);
    Assertions.assertEquals(MerchantStatusType.PURE_DELIVERY, BulkCreationCommonUtil.getMerchantType(profileResponse));

    Assertions.assertEquals(MerchantStatusType.PURE_DELIVERY, BulkCreationCommonUtil.getMerchantType(new ProfileResponse()));
  }

  @Test
  public void validateDeliveryStatusByMerchantTypeTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.BFB_STATUS_HEADER, "1");
    row.put(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_ID, "1");
    row.put(BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME, "1");
    row.put(BulkCnCreationHeaderNames.ROW_NUMBER, 1);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Dikirimkan oleh seller");


    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.PURE_DELIVERY,
        false, primaryIdentifier, false);
    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC,
        false, primaryIdentifier, false);
    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB,
        false, primaryIdentifier, false);
    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC,
        false, primaryIdentifier, false);
    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC,
        true, primaryIdentifier, false);
    row.put(BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME, "Bopis");
    BulkCreationCommonUtil.validateDeliveryStatusByMerchantType(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC, true,
        primaryIdentifier, false);
  }

  @Test
  public void validateBfbBasePriceTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "10.0");
    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "10.0", "");

    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "10.9999");
    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "10.9999",
        BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN);
    Assertions.assertEquals("11", String.valueOf(row.get(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN)));

    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "-1");
    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "-1", "");
    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "0");
    Pair<StringBuilder, Boolean> validateBfbBasePrice =
        BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
            new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "0", "");
    Assertions.assertEquals(new StringBuilder().append("Bfb base price should be greater than 0. ").toString(),
        validateBfbBasePrice.getLeft().toString());

    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "price");
    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "price", "");

    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "");
    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.BFB, "", "");

    BulkCreationCommonUtil.validateBfbBasePrice(row, DEFAULT_PRODUCT_NAME, "1", new BulkProcessNotes(),
        new BulkUploadErrorCounter(), true, new StringBuilder(), true, MerchantStatusType.PURE_DELIVERY, null, "");
  }

  @Test
  public void setB2bFieldInProductCreationRequestTest() {
    Map<String, String> rawBfbValues = new HashMap<>();
    rawBfbValues.put(BFB_BASE_PRICE, "");
    rawBfbValues.put(BFB_MANAGED, "");
    rawBfbValues.put(BFB_STATUS, "");
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.BFB_STATUS_HEADER, "");
    row.put(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, "");
    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "");
    BulkCreationCommonUtil.setB2bFieldInProductCreationRequest(MerchantStatusType.BFB, pickupPointCreateRequest,
        rawBfbValues);
    Assertions.assertFalse(pickupPointCreateRequest.getB2bFields().isBuyable());
    Assertions.assertFalse(pickupPointCreateRequest.getB2bFields().isDisplay());
    Assertions.assertFalse(pickupPointCreateRequest.getB2bFields().isManaged());
    Assertions.assertNull(pickupPointCreateRequest.getB2bFields().getPrice());

    row.put(BulkCnCreationHeaderNames.BFB_STATUS_HEADER, "1");
    row.put(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, "1");
    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, "1000");
    rawBfbValues.put(BFB_BASE_PRICE, "1000");
    rawBfbValues.put(BFB_MANAGED, "1");
    rawBfbValues.put(BFB_STATUS, "1");
    BulkCreationCommonUtil.setB2bFieldInProductCreationRequest(MerchantStatusType.BFB, pickupPointCreateRequest,
        rawBfbValues);
    Assertions.assertTrue(pickupPointCreateRequest.getB2bFields().isBuyable());
    Assertions.assertTrue(pickupPointCreateRequest.getB2bFields().isDisplay());
    Assertions.assertTrue(pickupPointCreateRequest.getB2bFields().isManaged());
    Assertions.assertEquals(1000.0, pickupPointCreateRequest.getB2bFields().getPrice().doubleValue(), 0);

    pickupPointCreateRequest.setB2bFields(null);
    BulkCreationCommonUtil.setB2bFieldInProductCreationRequest(MerchantStatusType.PURE_DELIVERY,
        pickupPointCreateRequest, rawBfbValues);
    Assertions.assertNull(pickupPointCreateRequest.getB2bFields());
  }

  @Test
  public void validateBfbManagedStatusTest() {
    Object rawObject = "1";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateBfbManagedStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateBfbManagedStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "2";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateBfbManagedStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "price";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateBfbManagedStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));
  }

  @Test
  public void validateBfbBuyableStatusTest() {
    Object rawObject = "1";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateBfbBuyableStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateBfbBuyableStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "2";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateBfbBuyableStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));

    rawObject = "price";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateBfbBuyableStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true));
  }

  @Test
  public void validateCncStatusTest() throws JsonProcessingException {
    Object rawObject = "1";
    HashMap<String, String> args = new HashMap<>();
    args.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(true));
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC.getType(),
            false, false));

    rawObject = "";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC.getType(),
            false, false));

    rawObject = "2";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC.getType(),
            false, false));

    rawObject = "price";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.BFB_AND_CNC.getType(),
            false, false));

    rawObject = "1";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            false, false));

    rawObject = "";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            false, false));

    rawObject = "2";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            false, false));

    rawObject = "price";
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            false, false));

    rawObject = "price";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.PURE_DELIVERY.getType(),
            false, false));

    rawObject = "0";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            true, false));

    rawObject = "1";
    primaryIdentifier = mapper.writeValueAsString(args);
    Assertions.assertFalse(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            true, true));

    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            true, false));

    args.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(false));
    rawObject = "0";
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            true, true));

    primaryIdentifier = PRODUCT_CODE;
    Assertions.assertTrue(
        BulkCreationCommonUtil.validateCncStatus(rawObject, new BulkProcessNotes(), new BulkUploadErrorCounter(),
            true, "1", DEFAULT_PRODUCT_NAME, new StringBuilder(), true, MerchantStatusType.DELIVERY_AND_CNC.getType(),
            true, true));
  }

  @Test
  public void getItemAttributeRequestTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    attributeResponse1.setName(ATTRIBUTE_CODE_1);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setVariantCreation(true);
    List<ProductItemAttributeValueRequest> productItemAttributeValueRequests =
        BulkCreationCommonUtil.getItemAttributeRequest(row, new ProductCreationRequest(), attributeResponse1, true);
    Assertions.assertEquals(ATTRIBUTE_VALUE, productItemAttributeValueRequests.get(0).getValue());

    productItemAttributeValueRequests =
        BulkCreationCommonUtil.getItemAttributeRequest(row, new ProductCreationRequest(), attributeResponse1, false);
    Assertions.assertTrue(productItemAttributeValueRequests.isEmpty());

    productItemAttributeValueRequests =
        BulkCreationCommonUtil.getItemAttributeRequest(row, new ProductCreationRequest(), null, true);
    Assertions.assertTrue(productItemAttributeValueRequests.isEmpty());

  }

  @Test
  public void getPickupPointCodeFromPickupPointCodeAndNameTest() {
    String result = BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(PICKUP_POINT_NAME_DELIMITER,
        PICKUP_POINT_CODE_AND_NAME);
    Assertions.assertEquals(PICKUP_POINT_CODE, result);
    String response = BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(PICKUP_POINT_NAME_DELIMITER,
        PICKUP_POINT_CODE);
    Assertions.assertEquals(response, PICKUP_POINT_CODE);
  }

  @Test
  public void getWareHouseCodeFromWareHouseCodeAndNameTest() {
    String result = BulkCreationCommonUtil
        .getWareHouseCodeFromWareHouseCodeAndName(PICKUP_POINT_NAME_DELIMITER, PICKUP_POINT_CODE_AND_NAME);
    Assertions.assertEquals(PICKUP_POINT_CODE, result);
    String response = BulkCreationCommonUtil
        .getWareHouseCodeFromWareHouseCodeAndName(PICKUP_POINT_NAME_DELIMITER, PICKUP_POINT_CODE);
    Assertions.assertEquals(response, PICKUP_POINT_CODE);
  }

  @Test
  public void initializeVariantCreationAttributesAndItemsTest() throws Exception {
    String ITEM_SKU = "item-sku";
    String ITEM_CODE = "item-code";
    Double OFFER_PRICE = 100.0;
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Arrays.asList(productAttributeDetailDTO));
    productAttributeDTO.setItemSku(ITEM_SKU);
    ProductInfoResponse product = new ProductInfoResponse();
    ProductVo productVo = new ProductVo();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_VALUE);
    productAttribute.setItemSku(ITEM_SKU);
    productAttribute.setProductAttributeDetails(Arrays.asList(productAttributeDetail));
    productVo.setDefiningAttributes(Arrays.asList(productAttribute));
    productVo.setMarkForDelete(false);
    productVo.setArchived(false);
    productVo.setProductName(PRODUCT_NAME);
    productVo.setCategoryCode(CATEGORY_CODE);

    ProductAndItemInfoResponseV2 productAndItemsResponse = new ProductAndItemInfoResponseV2();
    List<ProductAttributeDTO> definingAttributes = new ArrayList<>();
    definingAttributes.add(productAttributeDTO);
    Field definingAttributesField = ProductInfoResponse.class.getDeclaredField("definingAttributes");
    definingAttributesField.setAccessible(true);
    definingAttributesField.set(product, definingAttributes);
    Field productField = ProductAndItemInfoResponseV2.class.getDeclaredField("product");
    productField.setAccessible(true);
    productField.set(productAndItemsResponse, product);
    ItemInfoResponseV2 itemResponse = new ItemInfoResponseV2();
    itemResponse.setArchived(false);
    itemResponse.setItemSku(ITEM_SKU);
    itemResponse.setItemCode(ITEM_CODE);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(OFFER_PRICE);
    productAndItemsResponse.setItem(itemResponse);
    productAndItemsResponse.setProduct(product);
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    Map<String, LinkedHashMap<String, Object>> requestItems = new HashMap<>();
    LinkedHashMap<String, Object> linkedHashMap = new LinkedHashMap<>();
    linkedHashMap.put(StoreCopyConstants.FIELD_STOCK, 10);
    linkedHashMap.put(StoreCopyConstants.FIELD_STATUS, 1);
    linkedHashMap.put(StoreCopyConstants.FIELD_SHIPPING_TYPE, "Dikirimkan oleh seller");
    linkedHashMap.put(StoreCopyConstants.FIELD_PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    linkedHashMap.put(StoreCopyConstants.FIELD_MINIMUM_STOCK, 0);
    linkedHashMap.put(StoreCopyConstants.FIELD_OFFER_PRICE, 100.0);
    linkedHashMap.put(StoreCopyConstants.FIELD_LIST_PRICE, 100.0);
    requestItems.put(ITEM_SKU, linkedHashMap);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE);
    Set<ProductItemResponse> productAndItemsResponseSet = new HashSet<>();
    productAndItemsResponseSet.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productAndItemsResponseSet);
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    BulkCreationCommonUtil.initializeVariantCreationAttributesAndItems(productDetailResponse,
      existingAttributes, productCreationRequest, productAndItemsResponse, Arrays.asList(productAndItemsResponse), requestItems,
      profileResponse, false, true);
  }

  @Test
  public void initializeVariantCreationAttributesAndItemsWithValueTypeAdditionFalseTest() throws Exception {
    String ITEM_SKU = "item-sku";
    String ITEM_CODE = "item-code";
    Double OFFER_PRICE = 100.0;
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Arrays.asList(productAttributeDetailDTO));
    productAttributeDTO.setItemSku(ITEM_SKU);
    ProductInfoResponse product = new ProductInfoResponse();
    ProductVo productVo = new ProductVo();
    ProductAttribute productAttribute = new ProductAttribute();
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_VALUE);
    productAttribute.setItemSku(ITEM_SKU);
    productAttribute.setProductAttributeDetails(Arrays.asList(productAttributeDetail));
    productVo.setDefiningAttributes(Arrays.asList(productAttribute));
    productVo.setMarkForDelete(false);
    productVo.setArchived(false);
    productVo.setProductName(PRODUCT_NAME);
    productVo.setCategoryCode(CATEGORY_CODE);

    ProductAndItemInfoResponseV2 productAndItemsResponse = new ProductAndItemInfoResponseV2();
    List<ProductAttributeDTO> definingAttributes = new ArrayList<>();
    definingAttributes.add(productAttributeDTO);
    Field definingAttributesField =
        ProductInfoResponse.class.getDeclaredField("definingAttributes");
    definingAttributesField.setAccessible(true);
    definingAttributesField.set(product, definingAttributes);
    Field productField = ProductAndItemInfoResponseV2.class.getDeclaredField("product");
    productField.setAccessible(true);
    productField.set(productAndItemsResponse, product);
    ItemInfoResponseV2 itemResponse = new ItemInfoResponseV2();
    itemResponse.setArchived(false);
    itemResponse.setItemSku(ITEM_SKU);
    itemResponse.setItemCode(ITEM_CODE);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(OFFER_PRICE);
    productAndItemsResponse.setItem(itemResponse);
    productAndItemsResponse.setProduct(product);
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    Map<String, LinkedHashMap<String, Object>> requestItems = new HashMap<>();
    LinkedHashMap<String, Object> linkedHashMap = new LinkedHashMap<>();
    linkedHashMap.put(StoreCopyConstants.FIELD_STOCK, 10);
    linkedHashMap.put(StoreCopyConstants.FIELD_STATUS, 1);
    linkedHashMap.put(StoreCopyConstants.FIELD_SHIPPING_TYPE, "Dikirimkan oleh seller");
    linkedHashMap.put(StoreCopyConstants.FIELD_PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    linkedHashMap.put(StoreCopyConstants.FIELD_MINIMUM_STOCK, 0);
    linkedHashMap.put(StoreCopyConstants.FIELD_OFFER_PRICE, 100.0);
    linkedHashMap.put(StoreCopyConstants.FIELD_LIST_PRICE, 100.0);
    requestItems.put(ITEM_SKU, linkedHashMap);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE);
    Set<ProductItemResponse> productAndItemsResponseSet = new HashSet<>();
    productAndItemsResponseSet.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productAndItemsResponseSet);
    for (ProductAttributeResponse productAttributeResponse :
        productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    BulkCreationCommonUtil.initializeVariantCreationAttributesAndItems(productDetailResponse,
        existingAttributes, productCreationRequest, productAndItemsResponse,
        Arrays.asList(productAndItemsResponse), requestItems, profileResponse, false, false);
  }

  @Test
  public void initializeVariantCreationAttributesAndItemsNotEligibleTest() throws Exception {
    String ITEM_SKU = "item-sku";
    String ITEM_CODE = "item-code";
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    productAttributeDTO.setItemSku(ITEM_SKU);
    ProductInfoResponse product = new ProductInfoResponse();

    product.setProductSpecialAttributes(new ArrayList<>());
    ProductAndItemInfoResponseV2 productAndItemsResponse = new ProductAndItemInfoResponseV2();
    ItemInfoResponseV2 itemResponse = new ItemInfoResponseV2();
    itemResponse.setArchived(false);
    itemResponse.setItemSku(ITEM_SKU);
    itemResponse.setItemCode(ITEM_CODE);
    productAndItemsResponse.setItem(itemResponse);
    productAndItemsResponse.setProduct(product);
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    Map<String, LinkedHashMap<String, Object>> requestItems = new HashMap<>();
    LinkedHashMap<String, Object> linkedHashMap = new LinkedHashMap<>();
    linkedHashMap.put(StoreCopyConstants.FIELD_STOCK, 10);
    linkedHashMap.put(StoreCopyConstants.FIELD_STATUS, 1);
    linkedHashMap.put(StoreCopyConstants.FIELD_SHIPPING_TYPE, "Dikirimkan oleh seller");
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    requestItems.put(ITEM_SKU, linkedHashMap);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE);
    Set<ProductItemResponse> productAndItemsResponseSet = new HashSet<>();
    productAndItemsResponseSet.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productAndItemsResponseSet);
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    BulkCreationCommonUtil.initializeVariantCreationAttributesAndItems(productDetailResponse,
      existingAttributes, productCreationRequest, productAndItemsResponse, Arrays.asList(productAndItemsResponse), requestItems,
      profileResponse, true, false);
  }

  @Test
  public void checkBpBopisEligibilityTest() throws Exception {

    Map<String, Object> data = getInputRowData(FILE_NAME);
    data.put(GenericBulkHeaders.PRODUCT_TYPE,
      BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());

    //productType=1, profile.bigProduct=true, profile.bopis=true
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    boolean result =
        BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
            profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=1, profile.bigProduct=true, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());

    //productType=1, profile.bigProduct=false, profile.bopis=true
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=1, profile.bigProduct=false, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=2, profile.bigProduct=true, profile.bopis=true
    data.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getdescription());
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=2, profile.bigProduct=true, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=2, profile.bigProduct=false, profile.bopis=true
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertFalse(result);

    //productType=2, profile.bigProduct=false, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertFalse(result);

    //productType=3, profile.bigProduct=true, profile.bopis=true
    data.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=3, profile.bigProduct=true, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertFalse(result);

    //productType=3, profile.bigProduct=false, profile.bopis=true
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    //productType=3, profile.bigProduct=false, profile.bopis=false
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertFalse(result);

    //productType=3, profile.bigProduct=null, profile.bopis=null
    profileResponse.setBigProductFlag(null);
    profileResponse.setBopisFlag(null);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, false, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    category = new CategoryDetailResponse();
    category.setBopisEligible(true);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    category.setBopisEligible(false);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, PRODUCT_CODE, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    category.setBopisEligible(false);
    profileResponse.getCompany().setMerchantType(CM);
    HashMap<String, String> map = new HashMap<>();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(true));
    bulkProcess.setPrimaryIdentifier(mapper.writeValueAsString(map));
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertFalse(result);

    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(false));
    bulkProcess.setPrimaryIdentifier(mapper.writeValueAsString(map));
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    bulkProcess.setPrimaryIdentifier(null);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    bulkProcess.setPrimaryIdentifier(PRODUCT_CODE);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);

    category.setBopisEligible(false);
    data.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getdescription());
    profileResponse.getCompany().setMerchantType(CM);
    result = BulkCreationCommonUtil.checkBpBopisEligibility(data, bulkProcessNotes, bulkUploadErrorCounter, true,
        profileResponse, true, category, true, CM, bulkProcess.getPrimaryIdentifier());
    Assertions.assertTrue(result);
  }

  @Test
  public void testEvaluateProcessorConditions() {
    Assertions.assertTrue(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, true, true, true, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(false, true, true, true, true, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, false, true, true, true, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, false, true, true, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, true, false, true, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, true, true, false, true, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, true, true, true, false, true));
    Assertions.assertFalse(BulkCreationCommonUtil.evaluateProcessorConditions(true, true, true, true, true, true, false));
  }

  @Test
  public void getImageTypeTest(){
    String result = BulkCreationCommonUtil.getImageType("image/jpeg");
    Assertions.assertEquals("jpeg", result);
    result = BulkCreationCommonUtil.getImageType("image/png");
    Assertions.assertEquals("png", result);
    result = BulkCreationCommonUtil.getImageType("image/jpg");
    Assertions.assertEquals("jpg", result);
    result = BulkCreationCommonUtil.getImageType("image/webp");
    Assertions.assertEquals("webp", result);
    result = BulkCreationCommonUtil.getImageType("image/jpg;utf-charset=8");
    Assertions.assertEquals("jpg", result);
    result = BulkCreationCommonUtil.getImageType("invalid");
    Assertions.assertEquals(StringUtils.EMPTY, result);
  }

  @Test
  public void validateBundlingInfoTest() throws Exception {
    Map<String, Object> currRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currRow.put(BulkCnCreationHeaderNames.ROW_NUMBER, "5");
    boolean result =
        BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
            new StringBuilder(), true);
    Assertions.assertTrue(result);
    currRow.put(BulkCnCreationHeaderNames.CHILD_SKU_EN, "");
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertTrue(result);
    currRow.put(BulkCnCreationHeaderNames.CHILD_SKU_EN, VALID_ITEM_SKU);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "2");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertTrue(result);
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 0,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, INVALID_QUANTITY);
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "0");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
    currRow.put(BulkCnCreationHeaderNames.CHILD_SKU_EN, PRODUCT_NAME);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "2");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
    currRow.put(BulkCnCreationHeaderNames.CHILD_SKU_EN, "");
    result = BulkCreationCommonUtil.validateBundlingInfo(currRow, bulkProcessNotes, bulkUploadErrorCounter, true, 10,
        new StringBuilder(), true);
    Assertions.assertFalse(result);
  }

  @Test
  public void getBundleRecipeTest() throws Exception {
    Map<String, Object> currRow = getInputRowMapDataCn("generateInputRowDataMPPSwitchOn_Cn");
    currRow.put(BulkCnCreationHeaderNames.CHILD_SKU_EN, VALID_ITEM_SKU);
    currRow.put(BulkCnCreationHeaderNames.QUANTITY_EN, "2");
    Set<BundleRecipeRequest> bundleRecipeRequestSet = BulkCreationCommonUtil.getBundleRecipe(currRow);
    Assertions.assertEquals(VALID_ITEM_SKU, bundleRecipeRequestSet.iterator().next().getItemSku());
    Assertions.assertEquals(2, bundleRecipeRequestSet.iterator().next().getQuantity());
  }

  @Test
  public void getWorkOrderBulkProcess() throws Exception {
    BulkUpdateProcessDTO bulkWorkOrderDTO = new BulkUpdateProcessDTO();
    bulkWorkOrderDTO.setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    bulkWorkOrderDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    BulkProcess bulkProcess =
        BulkCreationCommonUtil.getWorkOrderBulkProcess(STORE_ID, REQUEST_ID, BULK_PROCESS_CODE, bulkWorkOrderDTO, 0, 0);
    Assertions.assertNotNull(bulkProcess);
  }

  @Test
  public void getWorkOrderBulkUpdateQueueTest(){
    BulkUpdateProcessDTO bulkWorkOrderDTO = new BulkUpdateProcessDTO();
    bulkWorkOrderDTO.setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    bulkWorkOrderDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkWorkOrderDTO.setClientHost(Category);
    bulkWorkOrderDTO.setPrivilegedMap(new HashMap<>());
    bulkWorkOrderDTO.setUpdatedBy(CREATED_BY);
    BulkUpdateQueue bulkUpdateQueue =
        BulkCreationCommonUtil.getWorkOrderBulkUpdateQueue(STORE_ID, REQUEST_ID, BULK_PROCESS_CODE, bulkWorkOrderDTO);
    Assertions.assertNotNull(bulkUpdateQueue);
  }

  @Test
  public void validateImageAndPrepareImageUrlAndLocationMapTest() {
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    bulkCreationCommonUtil.validateImageAndPrepareImageUrlAndLocationMap(bulkProcess, getImageUrlAndLocationMap(),
        Attribute1, getImageUrlAndLocationMap(), Arrays.asList(IMAGE_URL), new StringBuilder(),
        Map.of(IMAGE_URL, bulkProcessImage), IMAGE_URL);
    bulkCreationCommonUtil.validateImageAndPrepareImageUrlAndLocationMap(bulkProcess, getImageUrlAndLocationMap(),
        Attribute1, getImageUrlAndLocationMap(), new ArrayList<>(), new StringBuilder(),
        Map.of(IMAGE_URL, bulkProcessImage), IMAGE_URL);
    bulkProcessImage.setImageURL(IMAGE_URL_3);
    bulkCreationCommonUtil.validateImageAndPrepareImageUrlAndLocationMap(bulkProcess, getImageUrlAndLocationMap(),
        Attribute1, getImageUrlAndLocationMap(), new ArrayList<>(), new StringBuilder(),
        Map.of(IMAGE_URL, bulkProcessImage), IMAGE_URL);
  }

  @Test
  void isConvertedProductCreationUpload_True_Test() {
    Map<String, String> args = new HashMap<>();
    args.put(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), "true");
    boolean result = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    Assertions.assertTrue(result);
  }

  @Test
  void isConvertedProductCreationUpload_False_Test() {
    Map<String, String> args = new HashMap<>();
    args.put(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), "false");
    boolean result = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    Assertions.assertFalse(result);
  }

  @Test
  void isConvertedProductCreationUpload_Key_Not_There_Test() {
    Map<String, String> args = new HashMap<>();
    boolean result = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    Assertions.assertFalse(result);
  }

  @Test
  void isConvertedProductCreationUpload_Key_Null_Test() {
    Map<String, String> args = new HashMap<>();
    args.put(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), null);

    boolean result = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    Assertions.assertFalse(result);
  }

  @Test
  void isConvertedProductCreationUpload_Key_NonBooleanString() {
    Map<String, String> args = new HashMap<>();
    args.put(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), "yes");
    boolean result = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    Assertions.assertFalse(result);
  }

  @Test
  public void prepareMappingImageFilenamesMapTest() {
    ProductCreationRequest request = new ProductCreationRequest();
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    Map<String, String> mappingImageFilenames = new HashMap<>();
    bulkCreationCommonUtil.prepareMappingImageFilenamesMap(request, imageUrlAndLocationMap, AUTO_UPLOAD_URL_PREFIX,
        imageAndImageUrlReverseMap, mappingImageFilenames, IMAGE_JPG);
    Assertions.assertTrue(mappingImageFilenames.containsKey(IMAGE_JPG));
    Assertions.assertNull(mappingImageFilenames.get(IMAGE_JPG));
    Assertions.assertEquals(0, imageAndImageUrlReverseMap.size());
    Assertions.assertNull(request.getProductCreationType());
  }

  @Test
  public void prepareMappingImageFilenamesMap1Test() {
    ProductCreationRequest request = new ProductCreationRequest();
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    Map<String, String> mappingImageFilenames = new HashMap<>();
    bulkCreationCommonUtil.prepareMappingImageFilenamesMap(request, imageUrlAndLocationMap, AUTO_UPLOAD_URL_PREFIX,
        imageAndImageUrlReverseMap, mappingImageFilenames, StringUtils.EMPTY);
    Assertions.assertTrue(mappingImageFilenames.isEmpty());
    Assertions.assertTrue(imageAndImageUrlReverseMap.isEmpty());
    Assertions.assertNull(request.getProductCreationType());
  }

  @Test
  public void prepareMappingImageFilenamesMap2Test() {
    ProductCreationRequest request = new ProductCreationRequest();
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    Map<String, String> mappingImageFilenames = new HashMap<>();
    bulkCreationCommonUtil.prepareMappingImageFilenamesMap(request, imageUrlAndLocationMap, AUTO_UPLOAD_URL_PREFIX,
        imageAndImageUrlReverseMap, mappingImageFilenames, IMAGE_URL);
    Assertions.assertFalse(mappingImageFilenames.containsKey(IMAGE_URL));
    Assertions.assertTrue(mappingImageFilenames.isEmpty());
    Assertions.assertTrue(imageAndImageUrlReverseMap.isEmpty());
    Assertions.assertNull(request.getProductCreationType());
  }

  @Test
  public void prepareMappingImageFilenamesMap3Test() {
    ProductCreationRequest request = new ProductCreationRequest();
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    imageUrlAndLocationMap.put(IMAGE_URL, LOCATION);
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    Map<String, String> mappingImageFilenames = new HashMap<>();
    bulkCreationCommonUtil.prepareMappingImageFilenamesMap(request, imageUrlAndLocationMap, AUTO_UPLOAD_URL_PREFIX,
        imageAndImageUrlReverseMap, mappingImageFilenames, IMAGE_URL);
    Assertions.assertFalse(mappingImageFilenames.containsKey(IMAGE_URL));
    Assertions.assertTrue(mappingImageFilenames.containsKey(LOCATION));
    Assertions.assertNull(mappingImageFilenames.get(LOCATION));
    Assertions.assertEquals(1, imageAndImageUrlReverseMap.size());
    Assertions.assertEquals(IMAGE_URL, imageAndImageUrlReverseMap.get(LOCATION));
    Assertions.assertNull(request.getProductCreationType());
  }

  @Test
  public void prepareMappingImageFilenamesMap4Test() {
    ProductCreationRequest request = new ProductCreationRequest();
    Map<String, String> imageUrlAndLocationMap = new HashMap<>();
    imageUrlAndLocationMap.put(AUTO_UPLOAD_URL_IMAGE, LOCATION);
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    Map<String, String> mappingImageFilenames = new HashMap<>();
    bulkCreationCommonUtil.prepareMappingImageFilenamesMap(request, imageUrlAndLocationMap, AUTO_UPLOAD_URL_PREFIX,
        imageAndImageUrlReverseMap, mappingImageFilenames, AUTO_UPLOAD_URL_IMAGE);
    Assertions.assertFalse(mappingImageFilenames.containsKey(AUTO_UPLOAD_URL_IMAGE));
    Assertions.assertTrue(mappingImageFilenames.containsKey(LOCATION));
    Assertions.assertNull(mappingImageFilenames.get(LOCATION));
    Assertions.assertEquals(1, imageAndImageUrlReverseMap.size());
    Assertions.assertEquals(AUTO_UPLOAD_URL_IMAGE, imageAndImageUrlReverseMap.get(LOCATION));

  }

  @Test
  public void isValidHttpsImageTest() {
    imageUrlMap.put(PRODUCT_CODE, IMAGE_URL_1);
    Assertions.assertFalse(BulkCreationCommonUtil.isValidHttpsImage(PRODUCT_CODE, imageUrlMap));
  }

  @Test
  public void getRawBfbValuesTest() {
    Map<String, Object> row = new HashMap<>();
    row.put(BulkCnCreationHeaderNames.BFB_STATUS_HEADER, BFB_STATUS_VALUE);
    row.put(BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, BFB_MANAGED_VALUE);
    row.put(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, BFB_BASE_PRICE_VALUE);
    Map<String, String> rawBfbValues =
        BulkCreationCommonUtil.getRawBfbValues(row, BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_ID,
            BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_ID,
            BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, BulkCnCreationHeaderNames.BFB_STATUS_HEADER);
    Assertions.assertEquals(BFB_BASE_PRICE_VALUE, rawBfbValues.get(BFB_BASE_PRICE));
    Assertions.assertEquals(BFB_MANAGED_VALUE, rawBfbValues.get(BFB_STATUS));
    Assertions.assertEquals(BFB_STATUS_VALUE, rawBfbValues.get(BFB_MANAGED));
  }

  @Test
  public void getBulkInternalProcessTest(){
    BulkUpdateProcessDTO bulkUpdateProcessDTO = new BulkUpdateProcessDTO();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkUpdateProcessDTO.setBulkProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    BulkCreationCommonUtil.getBulkInternalProcess(bulkUpdateProcessDTO, bulkInternalProcess);
    Assertions.assertEquals(bulkInternalProcess.getProcessType(), BulkInternalProcessType.BULK_PRICE_REBATE.getValue());
  }

  @Test
  public void convertUserRowsToProductCollectionRequestsForBopisTes1t() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse
        .setPredefinedAllowedAttributeValues(getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.WARNA, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    userRow.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    ProductCreationRequest productCreationRequest = BulkCreationCommonUtil
        .convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse, getBulkProcess(),
            getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex, false,
            new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, false);
    Assertions.assertEquals(0.0, productCreationRequest.getHeight(), 0);
  }

  @Test
  public void convertUserRowsToProductCollectionRequestsForBopisInstoreSwtichOnTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse.setPredefinedAllowedAttributeValues(
        getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.WARNA, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    userRow.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse,
        getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex,
        false, new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, true);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
            attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
            attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
            MerchantStatusType.PURE_DELIVERY, "||", false, false);
    Assertions.assertEquals(0.0, productCreationRequest.getHeight(), 0);
  }

  @Test
  public void convertUserRowsToProductCollectionRequestsForBopisInstoreSwtichOnRegularProductTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse.setPredefinedAllowedAttributeValues(
        getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.WARNA, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    userRow.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse,
        getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex,
        false, new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, true);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
            attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
            attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
            MerchantStatusType.PURE_DELIVERY, "||", false, false);
    Assertions.assertEquals(10.0, productCreationRequest.getHeight(), 0);
  }

  @Test
  public void convertUserRowsToProductCollectionRequestsForPureInstoreSwtichOnRegularProductTest() throws Exception {
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("1");
    attributeResponse.setName("Dimensi Produk");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    attributeResponse.setPredefinedAllowedAttributeValues(
        getBrandAttributeResponse().getPredefinedAllowedAttributeValues());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .add(new CategoryAttributeResponse(attributeResponse, 1, true, true, STORE_ID));
    Map<String, Object> userRow = getInputRowData(FILE_NAME);
    userRow.put(GenericBulkHeaders.WARNA, "generated name");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    userRow.put(GenericBulkHeaders.CNC, "0");
    userRow.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
    userRow.put(GenericBulkHeaders.INSTORE, "1");
    userRow.put(GenericBulkHeaders.WEIGHT, "0");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1)
        .setAttribute(getBrandAttributeResponse());
    List<AttributeResponse> attributeResponses =
        genericCategoryDetailResponse.get(Category).getCategoryAttributes().stream()
            .map(CategoryAttributeResponse::getAttribute).collect(Collectors.toList());
    Map<String, AttributeResponse> attributeIdAndResponse =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getId, Function.identity()));
    Map<String, String> attributeNameAndId =
        attributeResponses.stream().collect(Collectors.toMap(AttributeResponse::getName, AttributeResponse::getId));
    Map<String, String> variantColumnIndex = new HashMap<>();
    variantColumnIndex.put("Warna", "Warna");
    BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow), attributeIdAndResponse,
        getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse, attributeNameAndId, variantColumnIndex,
        false, new GenerateShippingWeightRequest(), MerchantStatusType.PURE_DELIVERY, "||", false, true);
    userRow.put(GenericBulkHeaders.SALE_PRICE, "11");
    userRow.put(GenericBulkHeaders.STOCK, "");
    userRow.put(GenericBulkHeaders.INSTORE, "1");
    userRow.put(GenericBulkHeaders.WEIGHT, "0");
    userRow.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    userRow.put(GenericBulkHeaders.CNC, "0");
    ProductCreationRequest productCreationRequest =
        BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
            attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
            attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
            MerchantStatusType.PURE_DELIVERY, "||", false, true);
    Assertions.assertEquals(0.0, productCreationRequest.getHeight(), 0);
    userRow.put(GenericBulkHeaders.WEIGHT, "1");
    BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
            attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
            attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
            MerchantStatusType.PURE_DELIVERY, "||", false, true);
    userRow.put(GenericBulkHeaders.WEIGHT, "");
    BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
        attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
        attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
        MerchantStatusType.PURE_DELIVERY, "||", false, true);
    userRow.put(GenericBulkHeaders.WEIGHT, PRODUCT_NAME);
    productCreationRequest = BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(Arrays.asList(userRow),
        attributeIdAndResponse, getBulkProcess(), getProfileResponse(), genericCategoryDetailResponse,
        attributeNameAndId, variantColumnIndex, false, new GenerateShippingWeightRequest(),
        MerchantStatusType.PURE_DELIVERY, "||", false, true);
    Assertions.assertEquals(0.0, productCreationRequest.getHeight(), 0);
  }

  @Test
  public void getAttributeIdAndValuesMapTest() {
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setName(Constant.COLOUR_FAMILY);

    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse3.setName(Constant.BRAND);

    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE_2);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1 =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(ATTRIBUTE_VALUE);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    productAttributeValueResponse1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse1);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse3);
    productAttributeResponse1.setProductAttributeValues(Arrays.asList(productAttributeValueResponse1));

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse4);
    productAttributeResponse1.setProductAttributeValues(Arrays.asList(productAttributeValueResponse1));


    BulkCreationCommonUtil.getAttributeIdAndValuesMap(
        Arrays.asList(attributeResponse1, attributeResponse2, attributeResponse3, attributeResponse4),
        Map.of(ATTRIBUTE_CODE_1, productAttributeResponse1));
  }

  @Test
  public void getAttributeIdAndValuesMapAttributeNullTest() {
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setName(Constant.COLOUR_FAMILY);

    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse3.setName(Constant.BRAND);
    attributeResponse3.setMandatory(true);

    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE_2);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1 =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue(ATTRIBUTE_VALUE);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    productAttributeValueResponse1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse1);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse3);
    productAttributeResponse1.setProductAttributeValues(Arrays.asList(productAttributeValueResponse1));

    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse1.setAttribute(attributeResponse4);
    productAttributeResponse1.setProductAttributeValues(Arrays.asList(productAttributeValueResponse1));


    BulkCreationCommonUtil.getAttributeIdAndValuesMap(
        Arrays.asList(attributeResponse1, attributeResponse2, attributeResponse3, attributeResponse4),
        Map.of(ATTRIBUTE_CODE_2, productAttributeResponse2));
  }

  @Test
  public void getValueTypeByAllowedAttributeCodePrefixTest() {
    String prefix = BulkCreationCommonUtil.getValueTypeByAttributeCodeAndValue(
        Map.of(ATTRIBUTE_CODE_1,Map.of(ATTRIBUTE_VALUE, "US")), ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE);
    Assertions.assertEquals(prefix, "US");
  }

  @Test
  public void getValueTypeByAllowedAttributeCodePrefixTest2() {
    String prefix = BulkCreationCommonUtil.getValueTypeByAttributeCodeAndValue(
        Map.of(ATTRIBUTE_CODE_1, Map.of(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE)), ATTRIBUTE_CODE_1,
        ATTRIBUTE_VALUE);
    Assertions.assertNull(prefix);
  }

  @Test
  public void getAttributeCodeAndValueTypeMapTest() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setSizeAttribute(true);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productAttributeResponse.setAttribute(attributeResponse);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValueResponse =
        new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setValue(ATTRIBUTE_VALUE);
    allowedAttributeValueResponse.setValueType("US");
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponse));
    productDetailResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponse));
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        BulkCreationCommonUtil.getAttributeCodeAndValueTypeMap(productDetailResponse);
  }

  @Test
  public void getAttributeCodeAndValueTypeMapTest2() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeResponse.setSizeAttribute(true);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productAttributeResponse.setAttribute(attributeResponse);
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    productAttributeResponse.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponse));
    productDetailResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponse));
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        BulkCreationCommonUtil.getAttributeCodeAndValueTypeMap(productDetailResponse);
  }

  @Test
  public void getAttributeCodeAndValueTypeMapTestWithAttributeNull() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    productAttributeResponse.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponse));
    productDetailResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponse));
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        BulkCreationCommonUtil.getAttributeCodeAndValueTypeMap(productDetailResponse);
  }

  @Test
  public void setAttributeValueTypeMapTest() {
    TreeMap<String, String> attributesValueTypeMap = new TreeMap<>();
    BulkCreationCommonUtil.setAttributeValueTypeMap(true,
        Map.of(ATTRIBUTE_CODE_1, Map.of(ATTRIBUTE_VALUE, "US")), ATTRIBUTE_CODE_1,
        "ATTRIBUTE_VALUE", attributesValueTypeMap);
    Assertions.assertNull(attributesValueTypeMap.get(ATTRIBUTE_VALUE));
  }

  @Test
  public void setAttributeValueTypeMapTest2() {
    TreeMap<String, String> attributesValueTypeMap = new TreeMap<>();
    BulkCreationCommonUtil.setAttributeValueTypeMap(true,
        Map.of(ATTRIBUTE_CODE_1, Map.of(ATTRIBUTE_VALUE, "US")), ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE,
        attributesValueTypeMap);
    Assertions.assertNotNull(attributesValueTypeMap.get(ATTRIBUTE_CODE_1));
  }

  @Test
  public void getProductSalesChannelTest() throws Exception{
    Map<String, Object> rowData = getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2");
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "");
    BulkCreationCommonUtil.getProductSalesChannel(true, List.of(rowData), true);
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    rowData.put(GenericBulkHeaders.CNC, "0");
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    BulkCreationCommonUtil.getProductSalesChannel(true, List.of(rowData), true);
    rowData.put(GenericBulkHeaders.CNC, "1");
    BulkCreationCommonUtil.getProductSalesChannel(true, List.of(rowData), true);
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    rowData.put(GenericBulkHeaders.CNC, "0");
    rowData.put(GenericBulkHeaders.INSTORE, "0");
    Assertions.assertEquals(0, BulkCreationCommonUtil.getProductSalesChannel(true, List.of(rowData), true).size());
  }

  @Test
  public void isPureInstoreProductTest() throws Exception{
    Map<String, Object> rowData = getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2");
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "1");
    rowData.put(GenericBulkHeaders.CNC, "1");
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    Assertions.assertFalse(BulkCreationCommonUtil.isPureInstoreProduct(List.of(rowData), true, true));
    rowData.put(GenericBulkHeaders.CNC, "0");
    rowData.put(GenericBulkHeaders.INSTORE, "0");
    Assertions.assertFalse(BulkCreationCommonUtil.isPureInstoreProduct(List.of(rowData), true, true));
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    Assertions.assertTrue(BulkCreationCommonUtil.isPureInstoreProduct(List.of(rowData), true, true));
  }

  @Test
  public void getParagraphStringTest() {
    Assertions.assertEquals(StringUtils.EMPTY, BulkCreationCommonUtil.getParagraphString(""));
    Assertions.assertNotNull(BulkCreationCommonUtil.getParagraphString(PRODUCT_LEVEL3_PROCESSOR_DIR));
  }

  @Test
  public void checkIfPPCodeIsCncActivatedTest() {
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(false, new HashMap<>(), new ArrayList<>(), false);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, new HashMap<>(), new ArrayList<>(), false);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCncActivated(true);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, new HashMap<>(),
        Collections.singletonList(pickupPointResponse), false);
    pickupPointResponse.setCncActivated(false);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, new HashMap<>(),
        Collections.singletonList(pickupPointResponse), false);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, new HashMap<>(),
        Collections.singletonList(pickupPointResponse), false);
    HashMap<String, Object> hashMap = new HashMap<>();
    hashMap.put(GenericBulkHeaders.CNC, StringUtils.EMPTY);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, hashMap, Collections.singletonList(pickupPointResponse),
        true);
    hashMap = new HashMap<>();
    hashMap.put(GenericBulkHeaders.CNC_EN, StringUtils.EMPTY);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, hashMap, Collections.singletonList(pickupPointResponse),
        true);
    hashMap = new HashMap<>();
    hashMap.put(BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, hashMap, Collections.singletonList(pickupPointResponse),
        false);
    hashMap = new HashMap<>();
    hashMap.put(BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME, StringUtils.EMPTY);
    BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(true, hashMap, Collections.singletonList(pickupPointResponse),
        false);
  }

  @Test
  public void updateCncValueTest() {
    HashMap<String, Object> hashMap = new HashMap<>();
    hashMap.put(PRODUCT_CODE, StringUtils.EMPTY);
    BulkCreationCommonUtil.updateCncValue(hashMap, StringUtils.EMPTY, PRODUCT_CODE);
  }

  @Test
  void testSetDescriptionForGeneric_whenSuccessCountIsZero() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(10);
    bulkProcess.setDescription("bulk_upload_file.xlsx. Extra description");
    bulkProcess.setInternationalMerchant(false);
    int internalActivationPeriod = 5;
    int failedSize = 1;
    BulkCreationCommonUtil.setDescriptionForGeneric(bulkProcess, internalActivationPeriod, failedSize);
    String updatedDescription = bulkProcess.getDescription();
  }
  @Test
  void testSetDescriptionForCn_whenStatusIsNotPostLiveAndSuccessCountIsZero() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(10);
    bulkProcess.setDescription("testfile.xlsx. CN upload");
    bulkProcess.setInternationalMerchant(true);
    String status = "FAILED_STATUS";
    String slaNotificationMsg = "SLA info";
    BulkCreationCommonUtil.setDescriptionForCn(bulkProcess, status, slaNotificationMsg);
    String description = bulkProcess.getDescription();
  }

  @Test
  public void validateExcelAttributesTestIgnoreBrandCase() throws Exception {
    Map<String, List<String>> attributeValues = getAttributesIdAndPossibleValues();
    attributeValues.get(Attribute1).set(0, "Dimensi value");
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
      .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1).getAttribute().setMarkForDelete(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(3).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setName("Dimensi Produk");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute()
      .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setVariantCreation(false);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setNameEnglish("");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setName("Att");
    BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME_2), getBulkProcess(), bulkProcessNotes,
      bulkUploadErrorCounter, true, genericCategoryDetailResponse, getAttributesIdAndPossibleValues(),
      true);
  }

  @Test
  public void validateExcelAttributesTestIgnoreBrandCaseAttributeValueNullTest() throws Exception {
    Map<String, List<String>> attributeValues = getAttributesIdAndPossibleValues();
    attributeValues.get(Attribute1).set(0, "Dimensi value");
    Map<String, CategoryDetailAndShippingResponse> genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.get(Category).getCategoryAttributes()
        .addAll(getGenericCategoryDetailResponse().get(Category).getCategoryAttributes());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(1).getAttribute().setMarkForDelete(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(3).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setName("Dimensi Produk");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setId(Attribute1);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setVariantCreation(false);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(4).getAttribute().setNameEnglish("");
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setMandatory(true);
    genericCategoryDetailResponse.get(Category).getCategoryAttributes().get(7).getAttribute().setName("Att");
    Map<String, List<String>> attributeValues1 = getAttributesIdAndPossibleValues();
    attributeValues1.remove("Att-Id-2");
   boolean result = BulkCreationCommonUtil.validateExcelAttributes(getInputRowData(FILE_NAME_2), getBulkProcess(), bulkProcessNotes,
        bulkUploadErrorCounter, true, genericCategoryDetailResponse, attributeValues1,
        true);
    Assertions.assertFalse(result);
  }

  @Test
  void overrideBrandIgnoreCaseTest() {
    Map<String, Object> raw = new HashMap<>();
    List<String> stringList = new ArrayList<>();
    stringList.add(BRAND);
    BulkCreationCommonUtil.overrideBrandIgnoreCase(raw, stringList, BRAND, false);
  }
}
