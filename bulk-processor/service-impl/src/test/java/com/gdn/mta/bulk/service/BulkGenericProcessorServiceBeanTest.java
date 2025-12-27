package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
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
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;

public class BulkGenericProcessorServiceBeanTest {

  @InjectMocks
  private BulkGenericProcessorServiceBean bulkGenericProcessorServiceBean;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private GeneratorRepository generatorRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Object>> dataMapArgumentCaptor;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private BulkProcessImageService bulkProcessImageService;

  @Mock
  private ProtectedBrandValidationService protectedBrandValidationService;

  @Mock
  private PBPOutboundServiceBean pbpOutboundServiceBean;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private BulkCreationCommonUtil bulkCreationCommonUtil;

  @Mock
  private PickupPointService pickupPointService;

  private static final String STORE_ID = "10001";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String REQUEST_ID = "requestId";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String BULK_DATA = "bulkData1";
  private static final String BULK_DATA_1 = "bulkData2";
  private static final String BULK_DATA_2 = "bulkData3";
  private static final String inputBrandValue="ABC (IN_REVIEW)";

  private static final String inputBrandValue_1="ABC";
  private static final String ZIP_FILE = "zip_1";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String BULK_GENERIC_FILE_NAME = "Blibli mass upload template_v3.8.4.xlsm";
  private static final String BULK_GENERIC_FILE_NAME_EN = "BULK_GENERIC_FILE_NAME_EN";
  private static final String BULK_GENERIC_FILE_NAME_EN_2 = "BULK_GENERIC_FILE_NAME_EN_2";
  private static final String BULK_GENERIC_NEW_SHIPPING_TYPE = "Unified mass upload with new shipping type";
  private static final String BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE = "GenericExcelWithOptionalDefiningAttribute";
  private static final String BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE_2 = "GenericExcelWithOptionalDefiningAttribute2";
  private static final String BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE_3 = "GenericExcelWithOptionalDefiningAttribute3";
  private static final String BULK_GENERIC_WITH_NO_ZIP_FILE = "GenericExcelWithZipFile";
  private static final String INVALID_FILE = "InvalidDataInput";
  private static final String INVALID_FILE_1 = "InvalidDataInput1";
  private static final String INVALID_FILE_2 = "InvalidDataInput2";
  private static final String MPP = "MPP";
  private static final String CNC = "Cnc";
  private static final String INVALID_CATEGORY_FILE = "InvalidCategory";
  private static final String FAILED_UPC = "Failed-upc";
  private static final String ZIP_FILE_NAME = "ProductLevel3Processor.zip";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String CN_CATEGORY_KEYBOARD_ID = "C3-Id";
  private static final String DESCRIPTIVE_WARNA_ID = "Att-Id-1";
  private static final String BRAND_ID = "Att-Id-2";
  private static final String BRAND_ABC = "ABC";
  private static final String BRAND_APPROVED_STATUS = "APPROVED";
  private static final String BRAND_DRAFT_STATUS = "DRAFT";
  private static final String USP = "<p>USP</p>";
  private static final String PRODUCT_NAME = "Product name";
  private static final String BRAND_VALUE_ID = "Att-Brand-Value-Id";
  private static final String BRAND_CODE = "Att-Brand-Value-Code";
  private static final String URL_VIDEO = "URL Video";
  private static final String IMAGE_1 = "productCode/abc_product-name_full01.jpg";
  private static final String IMAGE_1_PNG = "productCode/abc_product-name_full01.png";
  private static final String IMAGE_2_PNG = "productCode/abc_product-name_full02.png";
  private static final String IMAGE_3 = "productCode/abc_product-name_full03.jpg";
  private static final String URL_IMAGE = "https://i.imgur.com/xCx7lQG.png";
  private static final String FAMILY_COLOUR_ID = "Att-Id-3";
  private static final String FAMILY_COLOUR_CODE = "Att-Code-3";
  private static final String VARIASI_ID = "Att-Id-4";
  private static final String FUNGSI = "Fungsi";
  private static final String WARNA = "Warna";
  private static final String COLOR = "Color";
  private static final String BRAND = "Brand";
  private static final String FUNGSI_VALUE = "Fungsi value";
  private static final String CATEGORY = "Strings";
  private static final String CATEGORY_CODE = "10418";
  private static final String FUNGSI_ATTRIBUTE_ID = "Att-Id-5";
  private static final String SKU_TRUE_LIAN_LIAN_ID = "Att-Id-6";
  private static final String SKU_TRUE_LIAN_LIAN_NAME = "Lain-lain";
  private static final String SKU_TRUE_LIAN_LIAN_CODE = "Att-Code-6";
  private static final String SKU_TRUE_LIAN_LIAN_VALUE = "Lain - lian value";
  private static final String FUNGSI_ATTRIBUTE_CODE = "Att-Code-5";
  private static final String DESCRIPTION = "<p>Description</p>";
  private static final String PICKUP_POINT_CODE = "PP-3000785";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String PRODUCT_CODE = "productCode";
  private static final int MAX_IMAGE_SIZE = 1024 * 1024;
  private static final String ATTRIBUTE_CODE_1 = "Att-Code-1";
  private static final String BLACK_VALUE = "Black";
  private static final String BLACK_VALUE_IN = "Hitam";
  private static final String SILVER_VALUE = "Silver";
  private static final String ITEM_NAME_1 = "Product name Black";
  private static final String ITEM_NAME_2 = "Product name Silver";
  private static final String SELLER_SKU_1 = "Seller Sku 1";
  private static final String SELLER_SKU_2 = "Seller Sku 2";
  private static final String EAN_1 = "12345";
  private static final String EAN_2 = "12345678";
  private static final String FAILED_USP = "ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template";
  public static final String HYPHEN = "-";
  private static final String TEMPLATE = "ProductLevel3Processor_Generic_Template_4";
  private static final String IMAGE_WITH_LESS_RESOLUTION = "ProductLevel3Processor_less_resolution_image.zip";
  private static final String TEMPLATE_1 = "ProductLevel3Processor_Generic_Template_8";
  private static final String IMAGE_WITH_INVALID_TYPE = "ProductLevel3Processor_Generic_Template 8.zip";
  private static String MAX_ROW_SIZE = "2";
  private static final String CC_MERCHANT = "CC";
  private static final String NOTES = "notes";

  private BulkCreateProductEventModel bulkCreateProductEventModel;
  private BulkProcessData bulkProcessData;
  private BulkProcessData bulkProcessData1;
  private BulkProcessData bulkProcessData2;
  private BulkProcessImage bulkProcessImage;
  private BulkProcessImage bulkProcessImage1;
  private BulkProcess bulkProcess;
  private SystemParameterConfig systemParameterConfig;
  private PickupPointResponse pickupPointResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);

    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfig.setValue("1");

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    bulkCreateProductEventModel =
        BulkCreateProductEventModel.builder().parentProduct(PARENT_PRODUCT).bulkProcessCode(BULK_PROCESS_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID).build();
    bulkProcessData = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(10)
        .bulkRequestData(BULK_DATA).status(BulkProcessData.STATUS_PENDING)
        .notes(StringUtils.EMPTY).build();
    bulkProcessData1 = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(8)
        .bulkRequestData(BULK_DATA_1).status(BulkProcessData.STATUS_PENDING)
        .notes(StringUtils.EMPTY).build();
    bulkProcessData2 = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(9)
        .bulkRequestData(BULK_DATA_2).status(BulkProcessData.STATUS_PENDING)
        .notes(NOTES).build();
    bulkProcessImage =
        BulkProcessImage.builder().imageURL(IMAGE_1).bulkProcessId(bulkProcess.getId()).location("Location").errorMessage("")
            .build();
    bulkProcessImage1 =
        BulkProcessImage.builder().imageURL(IMAGE_3).bulkProcessId(bulkProcess.getId()).location("Location").build();
    Mockito.when(systemParameter.getImageMaxSize()).thenReturn(MAX_IMAGE_SIZE);
    getExcelAndZipFiles(BULK_GENERIC_FILE_NAME, ZIP_FILE_NAME, ProcessorUtils.FILETYPE_XLSM_EXCEL);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, UPDATED_BY);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn(Arrays.asList(bulkProcessImage, bulkProcessImage1));
    downloadFile(URL_IMAGE, "image1015.png");
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "maxStockLimit", 10000000L);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "ppNameDelimiter", "||");
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.TRUE);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "eanUpcValidLength", Arrays.asList(5, 8, 12, 13));

    Mockito.when(protectedBrandValidationService.validateProtectedBrand(Mockito.anyMap(),
      Mockito.any(BulkProcess.class),Mockito.anyMap())).thenReturn(true);

    Mockito.when(
        fileStorageService.downloadImages(Mockito.anyString(), Mockito.anyMap(), Mockito.anyInt(),
          Mockito.anyInt(), Mockito.anySet(), Mockito.anyList(),
          Mockito.any(BulkUploadErrorCounter.class), Mockito.any(StringBuilder.class),
          Mockito.anyBoolean()))
      .thenReturn(ImageDownloadResult.builder().downloadSuccess(true).build());
    Mockito.when(
            bulkCreationCommonUtil.validateExcelImagesForSingleRow(Mockito.anyList(), Mockito.any(BulkProcess.class),
                Mockito.any(BulkUploadErrorCounter.class), Mockito.anyBoolean(),
                Mockito.anyMap(), Mockito.anyList(), Mockito.anyInt()))
        .thenReturn(StringUtils.EMPTY);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", false);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CM");
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "sizeChartValueTypeDelimiter", "-");
  }

  private void downloadFile(String url, String location) throws IOException {
    String path =
        ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE + File.separator + ProcessorUtils.DATA_RAW_DIR + File.separator
            + location;
    URL urlConnection = new URL(url);
    Path target = Paths.get(path);
    URLConnection connection = urlConnection.openConnection();
    connection.setConnectTimeout(5000);
    connection.setReadTimeout(10000);
    connection.setRequestProperty(Constant.ACCEPT_HEADER, Constant.IMAGE_HEADER);
    try (InputStream in = connection.getInputStream()) {
      if (url.equals(String.valueOf(connection.getURL()))) {
        Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void getExcelAndZipFiles(String excelFileName, String zipFileName, String excelFileType) throws Exception {
    Map<String, String> files = getGenericFiles(excelFileName, zipFileName);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSM));
    byte[] zipFile = Base64.decodeBase64(files.get(ZIP_FILE));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + bulkProcess
            .getBulkProcessCode() + excelFileType, excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcess.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR,
        zipFile);
  }

  private Map<String, String> getGenericFiles(String excelFileName, String zipFile) throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + excelFileName))), "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + zipFile))), "UTF-8");
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(bulkProcessRepository);
    Mockito.verifyNoMoreInteractions(pcbOutboundService);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(attributeRepository);
    Mockito.verifyNoMoreInteractions(generatorRepository);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(trackerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(bulkProcessDataService);
    Mockito.verifyNoMoreInteractions(bulkProcessImageService);
    Mockito.verifyNoMoreInteractions(pbpOutboundServiceBean);
    ProcessorUtils.deleteFile(ProcessorUtils.DATA_BASE_DIR);
  }

  private List<CategoryTreeResponse> getGenericCategoryTreeResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryTreePCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<List<CategoryTreeResponse>>() {
    });
  }

  private CategoryAttributeResponse getSkuTrueAttribute() {
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    categoryAttributeResponse.setStoreId(STORE_ID);
    categoryAttributeResponse.setMarkForDelete(false);
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attributeResponse.setAttributeCode(SKU_TRUE_LIAN_LIAN_CODE);
    attributeResponse.setName(SKU_TRUE_LIAN_LIAN_NAME);
    attributeResponse.setNameEnglish(SKU_TRUE_LIAN_LIAN_NAME);
    attributeResponse.setId(SKU_TRUE_LIAN_LIAN_ID);
    attributeResponse.setSkuValue(true);
    categoryAttributeResponse.setAttribute(attributeResponse);
    return categoryAttributeResponse;
  }

  private List<CategoryTreeResponse> getGenericCategoryTreeEnglishResponse() throws Exception {
    List<CategoryTreeResponse> genericCategoryTreeResponse = getGenericCategoryTreeResponse();
    genericCategoryTreeResponse.get(56).setCategoryName(CATEGORY);
    genericCategoryTreeResponse.get(56).getChildren().get(0).setCategoryName(CATEGORY);
    genericCategoryTreeResponse.get(56).setCategoryName(CATEGORY);
    genericCategoryTreeResponse.get(56).getChildren().get(0).getChildren().get(0).setCategoryName(CATEGORY);
    return genericCategoryTreeResponse;
  }

  private CategoryDetailAndShippingResponse getGenericCategoryDetailResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryDetailPCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<CategoryDetailAndShippingResponse>() {
    });
  }

  private AttributeResponse getBrandAttributeResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericBrandResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private AttributeResponse getFamilyColourResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericFamilyColourResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private AttributeResponse getDescriptiveWarnaResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericDescriptiveWarnaResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private AttributeResponse getDefiningWarnaResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericDefiningWarnaResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private AttributeResponse getDescriptiveVariasiResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericDescriptiveVariasiResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<AttributeResponse>() {
    });
  }

  private LinkedHashMap<String, Object> getInputRowData(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, typeRef);
  }

  private AttributeResponse getAttributeResponse(String id, String code, String name, String nameEnglish,
      String attributeType, boolean variantCreation, boolean skuValue) throws Exception {
    AttributeResponse attributeResponse = getDescriptiveVariasiResponse();
    attributeResponse.setId(id);
    attributeResponse.setAttributeCode(code);
    attributeResponse.setName(name);
    attributeResponse.setNameEnglish(nameEnglish);
    attributeResponse.setAttributeType(attributeType);
    attributeResponse.setVariantCreation(variantCreation);
    attributeResponse.setSkuValue(skuValue);
    return attributeResponse;
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

  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse() {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(1D);
    return generateShippingWeightResponse;
  }

  @Test
  public void processBulkGenericEventTest() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEventMPPSwitchOnTest() throws Exception {
    LinkedHashMap<String, Object> generateInputRowData = getInputRowData("generateInputRowDataMPPSwitchOn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Assertions.assertTrue(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isDisplay());
    Assertions.assertTrue(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isDisplay());
    Assertions.assertFalse(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isCncActive());
    Assertions.assertEquals(2,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getStock(), 0);
    Assertions.assertEquals(2,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getStock(), 0);
    Assertions.assertEquals(54321,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(54321,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getPickupPointId());
  }


  @Test
  public void processBulkGenericEvent_withAccessiblePickupPointCodes() throws Exception {
    LinkedHashMap<String, Object> generateInputRowData = getInputRowData("generateInputRowDataMPPSwitchOn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData2));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData2)).thenReturn(bulkProcessData2);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkProcess.setNotes(PICKUP_POINT_CODE);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }


  @Test
  public void processBulkGenericEvent_withPPCodes() throws Exception {
    LinkedHashMap<String, Object> generateInputRowData = getInputRowData("generateInputRowDataMPPSwitchOn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData2));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData2)).thenReturn(bulkProcessData2);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkProcess.setNotes(PICKUP_POINT_CODE);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEventMPPSwitchOnCncTest() throws Exception {
    LinkedHashMap<String, Object> generateInputRowData = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    pickupPointResponse.setCncActivated(true);
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Assertions.assertFalse(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isDisplay());
    Assertions.assertFalse(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isDisplay());
    Assertions.assertTrue(
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .isCncActive());
    Assertions.assertEquals(2,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getStock(), 0);
    Assertions.assertEquals(2,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getStock(), 0);
    Assertions.assertEquals(54321,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getPrice(), 0);
    Assertions.assertEquals(54321,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getSalePrice(), 0);
    Assertions.assertEquals(PICKUP_POINT_CODE,
        productCreationRequestArgumentCaptor.getValue().getProductItemRequests().get(0).getPickupPoints().get(0)
            .getPickupPointId());
  }

  @Test
  public void processBulkGenericEvent_excludedMerchantTypeTest() throws Exception {
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setMerchantType(CC_MERCHANT);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
      .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEvent_categoryDeactivatedTest() throws Exception {
    CategoryDetailAndShippingResponse categoryDetailAndShippingResponse =
      getGenericCategoryDetailResponse();
    categoryDetailAndShippingResponse.setActivated(false);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(getInputRowData("shippingWeightExceeded"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(categoryDetailAndShippingResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEventTest_SecondCategoryHierarchy() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("InvalidCategory_2"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    CategoryDetailAndShippingResponse categoryDetailAndShippingResponse = getGenericCategoryDetailResponse();
    categoryDetailAndShippingResponse.setId("Test-child-00022");
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId("Test-child-00022"))
        .thenReturn(categoryDetailAndShippingResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId("Test-child-00022");
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEventTestException() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.doThrow(Exception.class).when(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void processBulkGenericEventTest_NullMinPrice() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(null);
    try {
      bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    } finally {
      Mockito.verify(bulkProcessDataService)
          .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
              BulkProcessData.STATUS_PENDING);
      Mockito.verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
      Mockito.verify(businessPartnerRepository)
          .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
      Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
      Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
      Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
      Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
      Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
      Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
      verify(bulkProcessDataService)
          .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
              BulkProcessData.STATUS_PENDING);
      verify(objectMapper).readValue(anyString(), any(TypeReference.class));
      verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    }
  }

  @Test
  public void testProcessWithNoVaraintCreationAttributes() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("ProductCreationWithoutVaraintCreationAttributes"));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().remove(2);
    genericCategoryDetailResponse.getCategoryAttributes().remove(0);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void testProcessWithSkuTrueValues() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter, times(1)).getMtaImageSource();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void testProcessWithUSPMoreThan400Characters() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(FAILED_USP));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData("ProductLevel3Processor_USP_Exceeded_Limit_Generic_Template_2"));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute()
        .setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    List<CategoryTreeResponse> categoryTreeResponses = getGenericCategoryTreeEnglishResponse();
    categoryTreeResponses.add(55, categoryTreeResponses.get(56));
    categoryTreeResponses.get(55).setCategoryName("Alat Musik");
    categoryTreeResponses.get(55).getChildren().get(0).setCategoryName("Aksesoris Alat Musik");
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(false);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(categoryTreeResponses);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void invalidInputTest() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void processBulkGenericEventTest_validationFail() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowDataLongDesc"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    CategoryDetailAndShippingResponse categoryDetailAndShippingResponse = getGenericCategoryDetailResponse();
    categoryDetailAndShippingResponse.getCategoryAttributes().get(3).setMarkForDelete(true);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(categoryDetailAndShippingResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
  }

  @Test
  public void invalidCategoryInputTest() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_CATEGORY_FILE));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, BRAND_ID)).thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(bulkCreationCommonUtil).validateExcelImagesForSingleRow(anyList(), any(), any(),
        anyBoolean(), anyMap(), anyList(), anyInt());
    Mockito.verify(systemParameter).getImageMaxSize();
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void failedUpcTest() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(getInputRowData(FAILED_UPC));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute()
        .setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(false);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    List<CategoryTreeResponse> categoryTreeResponses = getGenericCategoryTreeEnglishResponse();
    categoryTreeResponses.add(55, categoryTreeResponses.get(56));
    categoryTreeResponses.get(55).setCategoryName("Alat Musik");
    categoryTreeResponses.get(55).getChildren().get(0).setCategoryName("Aksesoris Alat Musik");

    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(categoryTreeResponses);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void processBulkGenericEventTest_NoBulkData() throws Exception {
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList());
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void processBulkGenericEventTest_CreationError() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.doThrow(Exception.class).when(pbpOutboundServiceBean).createNewProduct(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .trackProductCreationFailure(Mockito.eq(BULK_PROCESS_CODE), Mockito.any(ProductCreationRequest.class),
            Mockito.any());
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void processBulkGenericEventTest_CreationErrorInstoreTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "instoreNewFlowEnabled", true);
    LinkedHashMap<String, Object> rowData = getInputRowData("generateInputRowData");
    rowData.put(GenericBulkHeaders.LENGTH, "0");
    rowData.put(GenericBulkHeaders.DELIVERY_STATUS, "0");
    rowData.put(GenericBulkHeaders.CNC, "0");
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(rowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.doThrow(Exception.class).when(pbpOutboundServiceBean).createNewProduct(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .trackProductCreationFailure(Mockito.eq(BULK_PROCESS_CODE), Mockito.any(ProductCreationRequest.class),
            Mockito.any());
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void processBulkGenericEventTest_CreationApplicationException() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.doThrow(ApplicationException.class).when(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .trackProductCreationFailure(Mockito.eq(BULK_PROCESS_CODE), Mockito.any(ProductCreationRequest.class),
            Mockito.any());
    Mockito.verify(systemParameterConfigService, Mockito.times(2))
        .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
  }

  @Test
  public void testProcessWithSkuTrueEnglishSpecialAttributeBlankValues() throws Exception {
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    CategoryAttributeResponse skuTrueAttribute = getSkuTrueAttribute();
    skuTrueAttribute.getAttribute().setName(StringUtils.EMPTY);
    skuTrueAttribute.getAttribute().setNameEnglish(StringUtils.EMPTY);

    CategoryAttributeResponse skuTrueAttribute1 = getSkuTrueAttribute();
    skuTrueAttribute1.getAttribute().setName(GenericBulkParameters.UKURAN);
    skuTrueAttribute1.getAttribute().setNameEnglish(GenericBulkParameters.UKURAN);
    skuTrueAttribute1.getAttribute().setAttributeCode(ATTRIBUTE_CODE_1);
    skuTrueAttribute1.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse skuTrueAttribute2 = getSkuTrueAttribute();
    skuTrueAttribute2.getAttribute().setName(GenericBulkParameters.VARIASI);
    skuTrueAttribute2.getAttribute().setNameEnglish(GenericBulkParameters.VARIASI);
    skuTrueAttribute2.getAttribute().setAttributeCode(VARIASI_ID);
    skuTrueAttribute2.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());


    genericCategoryDetailResponse.getCategoryAttributes().add(skuTrueAttribute);
    genericCategoryDetailResponse.getCategoryAttributes().add(skuTrueAttribute1);
    genericCategoryDetailResponse.getCategoryAttributes().add(skuTrueAttribute2);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessService).findByBulkProcessCode(bulkCreateProductEventModel.getStoreId(),
        bulkCreateProductEventModel.getBulkProcessCode());
    verify(objectMapper).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(
        bulkCreateProductEventModel.getStoreId(), bulkProcess, bulkCreateProductEventModel.getParentProduct(),
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(Mockito.anyList());
  }

  @Test
  public void testProcessWithSkuTrueValuesEnglishLanguage() throws Exception {
    downloadFile(URL_IMAGE, "image1014.png");
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation("image1014.png");
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn(Arrays.asList(bulkProcessImage));
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_FILE_NAME_EN));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_FILE_NAME_EN_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter, times(1)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void testProcessWithSkuTrueValuesEnglishLanguageAndDefiningWarna() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    downloadFile(URL_IMAGE, "image1014.png");
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation("image1014.png");
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn(Arrays.asList(bulkProcessImage));
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_FILE_NAME_EN));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_FILE_NAME_EN_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute()
        .setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter, times(1)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void testProcessWithNewShippingType() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    Mockito.verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void testProcessWithOptionalDefiningAttribute() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse descriptiveWarnaResponse = getDescriptiveWarnaResponse();
    descriptiveWarnaResponse.setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(descriptiveWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    Mockito.verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void testProcessWithNoZipFiles() throws Exception {
    downloadFile(URL_IMAGE, "image1014.png");
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation("image1014.png");
    Mockito.when(bulkProcessImageService.findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class)))
        .thenReturn(Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_WITH_NO_ZIP_FILE));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse descriptiveWarnaResponse = getDescriptiveWarnaResponse();
    descriptiveWarnaResponse.setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(descriptiveWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void testProcessWithImageTypeNotValidTest() throws Exception {
    getExcelAndZipFiles(TEMPLATE_1, IMAGE_WITH_INVALID_TYPE, ProcessorUtils.FILETYPE_XLSM_EXCEL);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(getInputRowData(TEMPLATE_1));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute()
        .setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(false);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    List<CategoryTreeResponse> categoryTreeResponses = getGenericCategoryTreeEnglishResponse();
    categoryTreeResponses.add(55, categoryTreeResponses.get(56));
    categoryTreeResponses.get(55).setCategoryName("Alat Musik");
    categoryTreeResponses.get(55).getChildren().get(0).setCategoryName("Aksesoris Alat Musik");
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(categoryTreeResponses);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());

  }

  @Test
  public void invalidInputTest_2() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void invalidInputTest_2MPPSwitchOn() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE + MPP));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_1 + MPP));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_2 + MPP));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void invalidInputTest_2MPPSwitchOnCnc() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE + MPP + CNC));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_1 + MPP + CNC));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_2 + MPP + CNC));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void invalidInputTest_2MPPSwitchOnEN() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE + MPP));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_1 + MPP));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_2 + MPP));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(false);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void invalidInputTest_2MPPSwitchOnCncEN() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE + MPP + CNC));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_1 + MPP + CNC));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class)))
        .thenReturn(getInputRowData(INVALID_FILE_2 + MPP + CNC));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setInternationalFlag(false);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void invalidInputTest_3() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(attributeRepository.addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID))
        .thenReturn(new AttributeValueResponse());
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void invalidInputTest_4() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    definingWarnaResponse.setVariantCreation(false);
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(attributeRepository.addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID))
        .thenReturn(new AttributeValueResponse());
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void invalidInputTest_6() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    definingWarnaResponse.setVariantCreation(false);
    definingWarnaResponse.setMandatory(false);
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(COLOR);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(attributeRepository.addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID))
        .thenReturn(new AttributeValueResponse());
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void invalidInputTest_5() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_1));
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE_2));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(attributeRepository.addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID))
        .thenReturn(new AttributeValueResponse());
    Mockito.when(systemParameter.getImageMaxSize()).thenReturn(1);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(attributeRepository, times(2))
        .addNewAttribute(BULK_PROCESS_CODE, SILVER_VALUE, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }

  @Test
  public void testProcessWithOptionalDefiningAttributeWith2Items() throws Exception {
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE_2));
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class)))
        .thenReturn(getInputRowData(BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE_3));
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse descriptiveWarnaResponse = getDescriptiveWarnaResponse();
    descriptiveWarnaResponse.setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(false);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    List<CategoryTreeResponse> categoryTreeResponses = getGenericCategoryTreeEnglishResponse();
    categoryTreeResponses.add(55, categoryTreeResponses.get(56));
    categoryTreeResponses.get(55).setCategoryName("Alat Musik");
    categoryTreeResponses.get(55).getChildren().get(0).setCategoryName("Aksesoris Alat Musik");
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(categoryTreeResponses);
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(descriptiveWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }

  @Test
  public void testProcessWithNoAttributeValues() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("ProductCreationWithoutVaraintCreationAttributes"));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().remove(2);
    genericCategoryDetailResponse.getCategoryAttributes().remove(1);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(1).getAttribute().setName("Brand");
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, FUNGSI_ATTRIBUTE_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, FUNGSI_ATTRIBUTE_ID, inputBrandValue_1);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(this.generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    verify(productRepository, times(1)).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    verify(pbpOutboundServiceBean).createNewProduct(eq(bulkProcess.getRequestId()), eq(bulkProcess.getCreatedBy()),
        productCreationRequestArgumentCaptor.capture());
    verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter).getImageMaxSize();
    verify(systemParameter).getMtaImageSource();
  }

  @Test
  public void generateBulkProcessDataAndImageTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", true);
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        MerchantStatusType.PURE_DELIVERY, CC_MERCHANT, new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(dataMapArgumentCaptor.capture());
    Assertions.assertEquals(map.get(GenericBulkHeaders.PICKUP_POINT_EN),
        dataMapArgumentCaptor.getValue().get(GenericBulkHeaders.PICKUP_POINT_EN));
  }

  @Test
  public void generateBulkProcessDataAndImageNonCncTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOn");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add(GenericBulkHeaders.CHILD_SKU_ID);
    userInputRows.add(GenericBulkHeaders.QUANTITY_ID);
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkProcess.setInternationalMerchant(false);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageNonCncTestEn() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOn");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add(GenericBulkHeaders.CHILD_SKU_ID);
    userInputRows.add(GenericBulkHeaders.QUANTITY_ID);
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkProcess.setInternationalMerchant(true);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(false);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.DELIVERY_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImage_Converted_Product_Creation_Test() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(false);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.DELIVERY_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false,
        Map.of(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), "true"));
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncBFBMerchantTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("B2B_PRICE");
    userInputRows.add("B2B_MANAGED");
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(false);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.BFB, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncBFBMerchantInternationalTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("B2B_PRICE");
    userInputRows.add("B2B_MANAGED");
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(true);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.BFB, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncBFBAndCncMerchantTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("B2B_PRICE");
    userInputRows.add("B2B_MANAGED");
    userInputRows.add("CNC");
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(false);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.BFB_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncBFBAndInternationalMerchantTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("B2B_PRICE");
    userInputRows.add("B2B_MANAGED");
    userInputRows.add("CNC");
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(true);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.BFB_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncTestEn() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkProcess.setInternationalMerchant(true);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.DELIVERY_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageCncFailedRowsNonEmptyTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(true);
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(getInputRowData(INVALID_FILE));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkProcess.setInternationalMerchant(true);
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
        Collections.singletonList(userInputRows), MerchantStatusType.DELIVERY_AND_CNC, CC_MERCHANT, new ArrayList<>(),
        new ArrayList<>(), List.of(1), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageISMerchantFalseTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_WITH_NO_ZIP_FILE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    bulkProcess.setInternationalMerchant(false);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig("A", "10", "B"));
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        MerchantStatusType.PURE_DELIVERY, CC_MERCHANT, new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(Mockito.anyList());
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE);
    verify(bulkProcessService).publishBulkImageDownloadEventModel(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowExceptionTest() throws Exception {
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture()))
      .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
      Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerExceptionTest() throws Exception {
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(false);
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
      .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture()))
      .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
      Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerExceptionInstoreTest() throws Exception {
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(false);
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture()))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), StringUtils.EMPTY, true, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
      Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerValidateExcelHeaderTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "genericFileHeaderValidationEn", true);
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture()))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_EN_CNC_MERCHANT),
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_CNC_MERCHANT), new ArrayList<>(),
              StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerValidateExcelHeaderBundleTest()
      throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "genericFileHeaderValidationEn", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_NON_CNC_MERCHANT),
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_EN_NON_CNC_MERCHANT),
              new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }


  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerValidHeaderTest()
      throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "genericFileHeaderValidationEn", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE)).thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    List<Object> excelBahasaHeaders = new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_NON_CNC_MERCHANT);
    excelBahasaHeaders.add(GenericBulkHeaders.CHILD_SKU_EN);
    excelBahasaHeaders.add(GenericBulkHeaders.QUANTITY_EN);
    List<Object> excelEnglishHeaders = new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_EN_NON_CNC_MERCHANT);
    excelEnglishHeaders.add(GenericBulkHeaders.CHILD_SKU_EN);
    excelEnglishHeaders.add(GenericBulkHeaders.QUANTITY_EN);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              excelBahasaHeaders, excelEnglishHeaders, new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
      Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void generateBulkProcessDataAndImageMaxRowNonInternationalSellerInvalidExcelHeaderTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "genericFileHeaderValidationEn", true);
    systemParameterConfig.setValue(String.valueOf(0));
    LinkedHashMap<String, Object> map = getInputRowData(BULK_GENERIC_NEW_SHIPPING_TYPE);
    List<Object> userInputRows = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
    }
    userInputRows.add("1234567890");
    userInputRows.add("0987654321");
    bulkProcess.setInternationalMerchant(false);
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
            SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture()))
        .thenReturn(bulkProcess);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE, "10000",
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE, "100",
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE, "10",
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> bulkGenericProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), MerchantStatusType.PURE_DELIVERY, CC_MERCHANT,
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_CNC_MERCHANT),
              new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_EN_CNC_MERCHANT),
              new ArrayList<>(), StringUtils.EMPTY, false, new HashMap<>()));
    } finally {
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    }
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeInternationMerchantTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    LinkedHashMap<String, Object> generateInputRowData =
      getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
      businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
      .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository
      .generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
      productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
      systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new ArrayList<>()).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService)
      .getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService, Mockito.times(1))
      .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeInternationMerchantOldFlowTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    LinkedHashMap<String, Object> generateInputRowData =
        getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
            .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
        .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
        .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
        .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
        .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository
            .generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
            productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
            .findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new ArrayList<>()).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService)
        .getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService, Mockito.times(1))
        .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    LinkedHashMap<String, Object> generateInputRowData =
      getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    Mockito.when(
      businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
      .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository
      .generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
      productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
      systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService)
      .getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService, Mockito.times(1))
      .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void testProcessWithBpBopisEligibilityTest1() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "bpBopisRestrictionEnabled",
      Boolean.TRUE);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
          BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1, bulkProcessData2));

    Map<String, Object> data = getInputRowData(INVALID_FILE);
    data.put(GenericBulkHeaders.PRODUCT_TYPE,"Bopis");
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(data);
    Map<String, Object> data1 = getInputRowData(INVALID_FILE_1);
    data1.put(GenericBulkHeaders.PRODUCT_TYPE,"Bopis");
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(data1);
    Map<String, Object> data2 = getInputRowData(INVALID_FILE_2);
    data2.put(GenericBulkHeaders.PRODUCT_TYPE,"Bopis");
    when(objectMapper.readValue(eq(BULK_DATA_2), any(TypeReference.class))).thenReturn(data2);
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);

    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.setBopisFlag(Boolean.FALSE);

    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    AttributeResponse definingWarnaResponse = getDefiningWarnaResponse();
    definingWarnaResponse.getAllowedAttributeValues().get(1).setValue(WARNA);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(definingWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
      getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_NAME,
        SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper, times(3)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID,
      bulkProcess, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(),
      any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
    Mockito.verify(attributeRepository, times(3))
      .addNewAttribute(BULK_PROCESS_CODE, "null", DESCRIPTIVE_WARNA_ID);
  }

  @Test
  public void testProcessWithBpBopisEligibilityTest2() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "bpBopisRestrictionEnabled",
      Boolean.TRUE);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    Map<String, Object> data = getInputRowData(BULK_GENERIC_OPTIONAL_DEFINING_ATTRIBUTE);
    data.put(GenericBulkHeaders.PRODUCT_TYPE_EN, "Bopis");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(data);
    CategoryDetailAndShippingResponse genericCategoryDetailResponse =
      getGenericCategoryDetailResponse();
    AttributeResponse descriptiveWarnaResponse = getDescriptiveWarnaResponse();
    descriptiveWarnaResponse.setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    profileResponse.setBopisFlag(Boolean.FALSE);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito.when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
      BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeEnglishResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID)).thenReturn(genericCategoryDetailResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1)).thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(descriptiveWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
      getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
        SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID,
      bulkProcess, PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(bulkProcessDataService, times(2)).saveBulkProcessData(anyList());
  }


  @Test
  public void validateAndFailMergedCellRowsTest() {
    List<String> headers = GenericBulkHeaders.HEADER_DATA_LIST_EN_NON_CNC_MERCHANT;
    List<Object> input = new ArrayList<>(GenericBulkHeaders.HEADER_DATA_LIST_EN_NON_CNC_MERCHANT);
    bulkGenericProcessorServiceBean.validateAndFailMergedCellRows(0, PARENT_PRODUCT, new ArrayList<>(),
        headers, new ArrayList<>(), new HashSet<>(), bulkProcess,
        bulkProcessData);
    bulkGenericProcessorServiceBean.validateAndFailMergedCellRows(0, StringUtils.EMPTY, new ArrayList<>(),
        headers, new ArrayList<>(), new HashSet<>(), bulkProcess,
        bulkProcessData);
    bulkGenericProcessorServiceBean.validateAndFailMergedCellRows(0, StringUtils.EMPTY, List.of(0),
        headers, new ArrayList<>(), new HashSet<>(), bulkProcess,
        bulkProcessData);
    bulkGenericProcessorServiceBean.validateAndFailMergedCellRows(0, PARENT_PRODUCT, new ArrayList<>(),
        headers, new ArrayList<>(), Set.of(PARENT_PRODUCT), bulkProcess,
        bulkProcessData);
    bulkGenericProcessorServiceBean.validateAndFailMergedCellRows(0, PARENT_PRODUCT, new ArrayList<>(),
        headers, input, Set.of(PARENT_PRODUCT), bulkProcess,
        bulkProcessData);
    Assertions.assertEquals(42, input.size());
  }

  @Test
  public void processBulkGenericEventOverridePickupPointTest() throws Exception {
    Page<PickupPointResponse> pageResponse =
      new PageImpl<>(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "overridePickupPointInCreation",
      Boolean.TRUE);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    LinkedHashMap<String, Object> generateInputRowData =
      getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    Mockito.when(
        pickupPointService.getSinglePickupPointSummaryFilter(Mockito.anyInt(),
         Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(pageResponse);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
          BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
      .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository
        .generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
        productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    Mockito.when(
        pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(new ArrayList<>()).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService)
      .getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService, Mockito.times(1))
      .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void processBulkGenericEventOverridePickupPointNotEligibleTest() throws Exception {
    Page<PickupPointResponse> pageResponse =
      new PageImpl<>(Arrays.asList(pickupPointResponse,pickupPointResponse));
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "overridePickupPointInCreation",
      Boolean.TRUE);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "variantAttributeSwitch", true);
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    LinkedHashMap<String, Object> generateInputRowData =
      getInputRowData("generateInputRowDataMPPSwitchOnCnc");
    Mockito.when(
        pickupPointService.getSinglePickupPointSummaryFilter(Mockito.anyInt(),
          Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(pageResponse);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(generateInputRowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
          BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setCncActivated(true);
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true))
      .thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository
        .generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
        productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    Mockito.when(
        pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
      .thenReturn(new ArrayList<>()).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService)
      .getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService, Mockito.times(1))
      .findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
    verify(systemParameter).getImageMaxSize();
  }

  @Test
  public void processBulkGenericEventBrandFetchIgnoreCaseTest() throws Exception {
    ReflectionTestUtils.setField(bulkGenericProcessorServiceBean, "fetchBrandIgnoreCaseInCreation", true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getGenericTemplateCategories(true, true)).thenReturn(getGenericCategoryTreeResponse());
    Mockito.when(pcbOutboundService.getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailByIdIgnoreCase(STORE_ID, BRAND_ID, inputBrandValue_1))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING))
      .thenReturn(systemParameterConfig);
    bulkGenericProcessorServiceBean.processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveBulkProcessData(anyList());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailByIdIgnoreCase(STORE_ID, BRAND_ID, inputBrandValue_1);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
      .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService, Mockito.times(2)).findValueByStoreIdAndVariable(eq(STORE_ID), anyString());
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessImageService).findByStoreIdAndBulkProcess(anyString(), any(BulkProcess.class));
  }
}
