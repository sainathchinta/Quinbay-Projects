package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

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
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;

public class ProductLevel3GenericProcessorServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String REQUEST_ID = "requestId";
  private static final String ZIP_FILE = "zip_1";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String BULK_GENERIC_FILE_NAME = "Blibli mass upload template_v3.8.4.xlsm";
  private static final String BULK_GENERIC_PRODUCT_CREATION_AUTO_UPLOAD =
      "GenericExcelAutoUploadProductCreation.xlsm";
  private static final String INVALID_FILE = "InvalidDataInput.xlsm";
  private static final String EXCEL_ARGUMENT = "excelFilename";
  private static final String ZIP_FILE_NAME = "ProductLevel3Processor.zip";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String CN_CATEGORY_KEYBOARD_ID = "C3-Id";
  private static final String DESCRIPTIVE_WARNA_ID = "Att-Id-1";
  private static final String BRAND_ID = "Att-Id-2";
  private static final String BRAND_ABC = "ABC";
  private static final String USP = "<p>USP</p>";
  private static final String PRODUCT_NAME = "Product name";
  private static final String BRAND_VALUE_ID = "Att-Brand-Value-Id";
  private static final String BRAND_CODE = "Att-Brand-Value-Code";
  private static final String FAMILY_COLOUR_ID = "Att-Id-3";
  private static final String FUNGSI = "Fungsi";
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
  private static final String PICKUP_POINT_CODE = "PP-3000785";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String PRODUCT_CODE = "productCode";
  private static final int MAX_IMAGE_SIZE = 1024 * 1024;
  private static final String BLACK_VALUE = "Black";
  private static final String SELLER_SKU_1 = "Seller Sku 1";
  private static final String EAN_1 = "12345";
  private static final String BULK_PROCESS_SUCCESS_MESSAGE_EN =
      " selesai diunggah. Sukses: 1 out of 1  produk(Product will be reviewed max 5 working days)";
  private static final String FINISHED_STATE = "FINISHED";
  private static final String ABORTED_STATE = "ABORTED";
  private static final String FILE_EMPTY_ERROR_MESSAGE = ". Excel File tidak boleh kosong";
  private static final String FAILED_MESSAGE_LINK = "link";
  public static final String HYPHEN = "-";
  private static final String AUTO_UPLOAD_STRING = "auto-upload-data";
  private static final String ACTIVE = "ACTIVE";
  private static final String UNIFIED_BASE_DIRECTORY = ClassLoader.getSystemClassLoader().getResource(
      org.apache.commons.lang.StringUtils.EMPTY).getPath() +  "/ExcelTemplate/";
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM = "Blibli-mass-base-Bfb-Cnc-template.xlsm";
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_WITHOUT_VERSION_XLSM =
      "Blibli-mass-base-Bfb-Cnc-template-without-version.xlsm";
  private static final String BLIBLI_MASS_UPLOAD_TEMPLATE_INCORRECT_VERSION_XLSM =
      "Blibli-mass-base-Bfb-Cnc-template-incorrect-version.xlsm";

  private static final String BLIBLI_MASS_UPLOAD_EMPTY_SHEET = "Blibli-mass-base-empty-sheet.xlsm";
  private static final String EXCEL_VERSION = "1.1";

  private BulkProcessQueue bulkProcessQueue;
  private BulkProcess bulkProcess;
  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigAutoUpload;
  private Map<String, String> protectedBrandNameCodeMap;

  @InjectMocks
  private ProductLevel3GenericProcessorServiceBean productLevel3GenericProcessorServiceBean;

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

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Mock
  private ProtectedBrandValidationService protectedBrandValidationService;

  @Mock
  private BulkGenericProcessorService bulkGenericProcessorService;

  @Mock
  private NotificationService notificationService;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setArgs(getArgs());
    bulkProcessQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessQueue.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcessQueue.setStoreId(STORE_ID);

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);

    Mockito.when(systemParameter.getImageMaxSize()).thenReturn(MAX_IMAGE_SIZE);

    Mockito.when(bulkProcessRepository
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);

    Mockito.when(systemParameter.getMtaImageSource())
        .thenReturn(Thread.currentThread().getContextClassLoader().getResource(PRODUCT_LEVEL3_PROCESSOR_DIR).getPath());
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfig.setValue("1");

    systemParameterConfigAutoUpload = new SystemParameterConfig();
    systemParameterConfigAutoUpload.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfigAutoUpload.setValue(AUTO_UPLOAD_STRING);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(systemParameterConfigAutoUpload);

    getExcelAndZipFiles(BULK_GENERIC_FILE_NAME, ZIP_FILE_NAME, ProcessorUtils.FILETYPE_XLSM_EXCEL);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, UPDATED_BY);

    protectedBrandNameCodeMap = new HashMap<>();
    protectedBrandNameCodeMap.put(BRAND, BRAND_CODE);
    Mockito.when(protectedBrandValidationService.fetchProtectedBrandNameCodeMap(STORE_ID)).thenReturn(protectedBrandNameCodeMap);
    Mockito.when(
        fileStorageServiceBean.downloadImages(anyString(), Mockito.anyMap(), Mockito.anyInt(),
          Mockito.anyInt(), Mockito.anySet(), Mockito.anyList(), any(BulkUploadErrorCounter.class),
          any(StringBuilder.class), Mockito.anyBoolean()))
      .thenReturn(ImageDownloadResult.builder().downloadSuccess(true).build());
    Mockito.when(
      fileStorageServiceBean.downloadAndValidateProductCreationImages(any(BulkProcess.class),
        any(BulkUploadErrorCounter.class), any(), any(StringBuilder.class), any(),
        anyString(), eq(1), eq(true))).thenReturn(true);
  }

  private void getExcelAndZipFiles(String excelFileName, String zipFileName, String excelFileType) throws Exception {
    Map<String, String> files = getGenericFiles(excelFileName, zipFileName);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSM));
    byte[] zipFile = Base64.decodeBase64(files.get(ZIP_FILE));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue
            .getBulkProcessCode() + excelFileType, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
  }

  private void getExcelWithoutZip(String excelFileName, String excelFileType) throws Exception {
    Map<String, String> files = getGenericFilesWithoutZip(excelFileName);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSM));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue
            .getBulkProcessCode() + excelFileType, excelFile);
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR);
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
    Mockito.verifyNoMoreInteractions(bulkFailedProductFileService);
    Mockito.verifyNoMoreInteractions(bulkGenericProcessorService);
    ProcessorUtils.deleteFile(ProcessorUtils.DATA_BASE_DIR);
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

  private Map<String, String> getGenericFilesWithoutZip(String excelFileName) throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + excelFileName))), "UTF-8");
    files.put("xlsm", excelData);
    return files;
  }

  private Map<String, String> getArgs() {
    Map<String, String> args = new HashMap<String, String>();
    args.put(EXCEL_ARGUMENT, BULK_GENERIC_FILE_NAME);
    args.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, UPDATED_BY);
    return args;
  }

  private List<CategoryTreeResponse> getGenericCategoryTreeResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryTreePCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<List<CategoryTreeResponse>>() {
    });
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
    profileResponse.setMerchantStatus(ACTIVE);
    return profileResponse;
  }

  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse() {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(1D);
    return generateShippingWeightResponse;
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

  @Disabled
  @Test
  public void readFileErrorTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, UPDATED_BY);
    bulkProcessQueue.getArgs().replace(EXCEL_ARGUMENT, INVALID_FILE);
    bulkProcess.setBulkProcessCode(INVALID_FILE);
    File file = new File("target/x-bulk/InvalidDataInput.xlsm/InvalidDataInput.xlsm");
    Mockito.when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(bulkProcessService, Mockito.times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            StringUtils.SPACE, TrackerConstants.FAILED, UPDATED_BY);
    Assertions.assertEquals(INVALID_FILE + FILE_EMPTY_ERROR_MESSAGE,
        bulkProcessArgumentCaptor.getValue().getDescription());
    Assertions.assertFalse(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertNull(bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertNull(bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertNull(bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(ABORTED_STATE, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  @Disabled
  public void testAutoUploadProductCreation() throws Exception {
    getExcelWithoutZip(BULK_GENERIC_PRODUCT_CREATION_AUTO_UPLOAD, ProcessorUtils.FILETYPE_XLSM_EXCEL);
    bulkProcessQueue.getArgs().replace(EXCEL_ARGUMENT, BULK_GENERIC_PRODUCT_CREATION_AUTO_UPLOAD);
    CategoryDetailAndShippingResponse genericCategoryDetailResponse = getGenericCategoryDetailResponse();
    AttributeResponse descriptiveWarnaResponse = getDescriptiveWarnaResponse();
    descriptiveWarnaResponse.setMandatory(false);
    genericCategoryDetailResponse.getCategoryAttributes().get(0).getAttribute().setMandatory(false);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    genericCategoryDetailResponse.getCategoryAttributes().add(getSkuTrueAttribute());
    Mockito.when(protectedBrandValidationService
      .validateProtectedBrandAuthorisation(anyString(), any(), any()))
      .thenReturn(true);
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
    Mockito.when(attributeRepository.findOne(STORE_ID, BRAND_ID)).thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(descriptiveWarnaResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI_ATTRIBUTE_CODE, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(attributeRepository.findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID)).thenReturn(
        getAttributeResponse(SKU_TRUE_LIAN_LIAN_ID, SKU_TRUE_LIAN_LIAN_CODE, SKU_TRUE_LIAN_LIAN_ID,
            SKU_TRUE_LIAN_LIAN_NAME, AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, true));
    Mockito.when(this.generatorRepository.generateShippingWeight(any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getUpdatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(bulkFailedProductFileService
        .createFile(any(), any(), any(), any(), Mockito.eq(false)))
        .thenReturn(FAILED_MESSAGE_LINK);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(protectedBrandValidationService,times(1)).fetchProtectedBrandNameCodeMap(STORE_ID);
    Mockito.verify(protectedBrandValidationService,times(1))
      .validateProtectedBrandAuthorisation(anyString(), any(), any());
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(bulkProcessService, Mockito.times(3)).saveOperation(bulkProcessArgumentCaptor.capture());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbOutboundService).getGenericTemplateCategories(true, true);
    Mockito.verify(pcbOutboundService).getCategoryInfoByCategoryId(CN_CATEGORY_KEYBOARD_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, BRAND_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, SKU_TRUE_LIAN_LIAN_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getUpdatedBy());
    Mockito.verify(productRepository)
        .createProduct(any(), any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter, times(2)).getImageMaxSize();
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    ProductCreationRequest creationRequest = productCreationRequestArgumentCaptor.getValue();
    Assertions.assertEquals(PRODUCT_CODE, creationRequest.getProductCode());
    Assertions.assertEquals(10, creationRequest.getLength(), 0);
    Assertions.assertEquals(10, creationRequest.getWidth(), 0);
    Assertions.assertEquals(10, creationRequest.getHeight(), 0);
    Assertions.assertEquals(0.1, creationRequest.getWeight(), 0);
    Assertions.assertTrue(creationRequest.getImages().get(0).isMainImages());
    Assertions.assertTrue(creationRequest.getImages().get(0).getOriginalImage());
    Assertions.assertEquals(CATEGORY, creationRequest.getProductCategories().get(0).getCategory().getName());
    Assertions.assertEquals(CATEGORY_CODE, creationRequest.getProductCategories().get(0).getCategory().getCategoryCode());
    Assertions.assertEquals(STORE_ID, creationRequest.getImages().get(0).getStoreId());
    Assertions.assertEquals(STORE_ID, creationRequest.getStoreId());
    Assertions.assertEquals(STORE_ID, creationRequest.getProductCategories().get(0).getStoreId());
    Assertions.assertEquals(PICKUP_POINT_CODE, creationRequest.getProductItemRequests().get(0).getPickupPointId());
    Assertions.assertEquals(PRODUCT_NAME, creationRequest.getProductItemRequests().get(0).getItemGeneratedName());
    Assertions.assertEquals(54321, creationRequest.getProductItemRequests().get(0).getPrice(), 0);
    Assertions.assertEquals(54321, creationRequest.getProductItemRequests().get(0).getSalePrice(), 0);
    Assertions.assertEquals(2, creationRequest.getProductItemRequests().get(0).getStock(), 0);
    Assertions.assertTrue(creationRequest.getProductItemRequests().get(0).isBuyable());
    Assertions.assertTrue(creationRequest.getProductItemRequests().get(0).isDisplay());
    Assertions.assertTrue(MapUtils.isEmpty(creationRequest.getProductItemRequests().get(0).getAttributesMap()));
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
        creationRequest.getProductAttributes().get(1).getAttribute().getAttributeType());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_NAME, creationRequest.getProductAttributes().get(1).getProductAttributeName());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_ID, creationRequest.getProductAttributes().get(1).getAttribute().getId());
    Assertions.assertEquals(BRAND, creationRequest.getProductAttributes().get(2).getProductAttributeName());
    Assertions.assertEquals(AttributeType.PREDEFINED_ATTRIBUTE,
        creationRequest.getProductAttributes().get(2).getAttribute().getAttributeType());
    Assertions.assertEquals(BRAND_ABC, creationRequest.getProductAttributes().get(2).getProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getValue());
    Assertions.assertEquals(BRAND_VALUE_ID, creationRequest.getProductAttributes().get(2).getProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getId());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_NAME, creationRequest.getProductAttributes().get(1).getProductAttributeName());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
        creationRequest.getProductAttributes().get(1).getAttribute().getAttributeType());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_VALUE,
        creationRequest.getProductAttributes().get(1).getProductAttributeValues().get(0)
            .getDescriptiveAttributeValue());
    Assertions.assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
        creationRequest.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(FUNGSI, creationRequest.getProductAttributes().get(0).getProductAttributeName());
    Assertions.assertEquals(FUNGSI_VALUE, creationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    Assertions.assertEquals(SELLER_SKU_1, creationRequest.getProductItemRequests().get(0).getMerchantSku());
    Assertions.assertEquals(EAN_1, creationRequest.getProductItemRequests().get(0).getUpcCode());
    Assertions.assertEquals(1, creationRequest.getProductItemRequests().get(0).getProductType(), 0);
    Assertions.assertEquals(2, creationRequest.getProductItemRequests().get(0).getImages().size());
    Assertions.assertTrue(CollectionUtils
        .isEmpty(creationRequest.getProductItemRequests().get(0).getProductItemAttributeValueRequests()));
    Assertions.assertTrue(creationRequest.getProductItemRequests().get(0).getImages().get(0).isMainImages());
    Assertions.assertTrue(creationRequest.getProductItemRequests().get(0).getImages().get(0).getOriginalImage());
    Assertions.assertEquals(1, creationRequest.getProductBusinessPartnerAttributes().size());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_ID,
        creationRequest.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    Assertions.assertEquals(SKU_TRUE_LIAN_LIAN_VALUE,
        creationRequest.getProductBusinessPartnerAttributes().get(0).getValue());
    Assertions.assertEquals(BULK_GENERIC_PRODUCT_CREATION_AUTO_UPLOAD + BULK_PROCESS_SUCCESS_MESSAGE_EN,
        bulkProcessArgumentCaptor.getValue().getDescription());
    Assertions.assertEquals(FINISHED_STATE, bulkProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getBulkProcessNotes().size(), 0);
    Assertions.assertFalse(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertTrue(creationRequest.isContainsUrlImage());
    Assertions.assertEquals(ProductCreationType.AUTO_UPLOAD, creationRequest.getProductCreationType());
  }


  @Test
  public void processNotificationHeaderValidationEnabledTest() throws Exception {
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Header File Excel tidak sesuai dengan template. Silakan periksa lagi dan upload"));
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
      true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
        TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationEnabledHeaderValidationTest() throws Exception {
    Mockito.when(
                    businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
            .thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN));
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
            true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
            .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
                    BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
            .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
                    TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationEnabledHeaderValidationIdTest() throws Exception {
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN));
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
        true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationEnabledHeaderValidationEnTest() throws Exception {
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN));
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
        true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationCompanyNullTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(new ProfileResponse());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationCompanyNotNullTest() throws Exception {
    BulkProcess bulkProcess1 = new BulkProcess();
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().company(new CompanyDTO()).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(bulkProcess1);
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("true", "true", "true"));
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    verify(bulkProcessService).saveOperation(bulkProcess1);
  }

  @Test
  public void processNotificationHeaderValidationEmptySheetTest() throws Exception {
    BulkProcess bulkProcess1 = new BulkProcess();
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes",
        "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().company(new CompanyDTO()).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(bulkProcess1);
    File file = new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_EMPTY_SHEET);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("true", "true", "true"));
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    verify(bulkProcessService).saveOperation(bulkProcess1);
    Assertions.assertEquals(BulkProcess.STATUS_IN_PROGRESS, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationCompanyNotNullActiveMerchantTest() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkGenericExcelVersion", "1.1");
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().merchantStatus("ACTIVE").company(new CompanyDTO()).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(new BulkProcess());
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("true", "true", "true"));
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(bulkGenericProcessorService).generateBulkProcessDataAndImage(any(), anyList(), any(), anyString(),
        anyList(), anyList(), eq(new ArrayList<>()), eq(StringUtils.EMPTY), eq(false), anyMap());
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_IN_PROGRESS, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationCompanyNotNullActiveMerchantBulkSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkGenericExcelVersion", "1.1");
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().merchantStatus("ACTIVE").company(new CompanyDTO()).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(new BulkProcess());
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("false", "false", "true"));
    bulkProcess.setInternationalMerchant(true);
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(), anyString());
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_IN_PROGRESS, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationWithoutVersionTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkGenericExcelVersion", "1.1");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkExcelVersioningEn", true);
    ProfileResponse profileResponse = ProfileResponse.builder().merchantStatus("ACTIVE").company(new CompanyDTO()).build();
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(new BulkProcess());
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_WITHOUT_VERSION_XLSM);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("false", "false", "true"));
    bulkProcess.setInternationalMerchant(true);
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationIncorrectVersionTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkGenericExcelVersion", "1.1");
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkExcelVersioningEn", true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().merchantStatus("ACTIVE").company(new CompanyDTO()).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    when(bulkProcessService.saveOperation(any())).thenReturn(new BulkProcess());
    File file =
        new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_INCORRECT_VERSION_XLSM);
    when(fileStorageServiceBean.downloadFile(any(), anyString())).thenReturn(FileUtils.readFileToByteArray(file));
    when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(
        new SystemParameterConfig("false", "false", "true"));
    bulkProcess.setInternationalMerchant(false);
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationEnabledHeaderValidationNoErrorTest() throws Exception {
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenThrow(new Exception());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
        true);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
            TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void processNotificationHeaderValidationDisabledTest() throws Exception {
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenThrow(new Exception());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck",
      false);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
      .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
        BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, " ",
        TrackerConstants.FAILED, UPDATED_BY);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());

  }

  @Test
  public void processNotificationHeaderValidation_Test() throws Exception {
    bulkProcess.setInternationalMerchant(true);
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "bulkGenericExcelVersion", "1.1");
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().company(new CompanyDTO()).merchantStatus(ACTIVE).build());
    ReflectionTestUtils.setField(productLevel3GenericProcessorServiceBean, "headerValidationCheck", true);
    Mockito.when(bulkProcessService.saveOperation(any())).thenReturn(new BulkProcess());
    File file = new File(UNIFIED_BASE_DIRECTORY + Constant.SLASH + BLIBLI_MASS_UPLOAD_TEMPLATE_XLSM);
    Mockito.when(fileStorageServiceBean.downloadFile(any(), anyString()))
        .thenReturn(FileUtils.readFileToByteArray(file));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(anyString(), anyString()))
        .thenReturn(new SystemParameterConfig("true", "true", "true"));
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    productLevel3GenericProcessorServiceBean.process(bulkProcessQueue);
    Mockito.verify(bulkProcessRepository)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE,
            BulkProcess.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(bulkProcessService).saveOperation(any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(bulkGenericProcessorService).generateBulkProcessDataAndImage(any(), anyList(), any(), anyString(),
        anyList(), anyList(), eq(new ArrayList<>()), eq(StringUtils.EMPTY), eq(false), anyMap());
  }

}