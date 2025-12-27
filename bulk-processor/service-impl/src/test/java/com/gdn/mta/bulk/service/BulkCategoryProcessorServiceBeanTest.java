package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

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
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.CnExcelHeaderNames;
import com.gdn.mta.bulk.config.SystemParameter;
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
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

public class BulkCategoryProcessorServiceBeanTest {

  @InjectMocks
  private BulkCategoryProcessorServiceBean bulkCategoryProcessorServiceBean;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private PBPOutboundServiceBean pbpOutboundServiceBean;

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
  private ProtectedBrandValidationService protectedBrandValidationService;

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
  private CategoryRepository categoryRepository;

  @Mock
  private FileStorageService fileStorageService;

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

  private static final String inputBrandValue="Palmerhaus";
  private static final String ZIP_FILE = "zip_1";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String BULK_GENERIC_FILE_NAME = "Blibli mass upload template_v3.8.4.xlsm";
  private static final String ZIP_FILE_NAME = "ProductLevel3Processor.zip";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String DESCRIPTIVE_WARNA_ID = "Att-Id-1";
  private static final String BRAND_ID = "Att-Id-2";
  private static final String FAMILY_COLOUR_ID = "Att-Id-3";
  private static final String FUNGSI = "Fungsi";
  private static final String CATEGORY_CODE = "10418";
  private static final String FUNGSI_ATTRIBUTE_ID = "Att-Id-5";
  private static final String FUNGSI_ATTRIBUTE_CODE = "Att-Code-5";
  private static final String PICKUP_POINT_CODE = "PP-3000785";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_NAME = "bpName";
  private static final String PRODUCT_CODE = "productCode";
  private static final int MAX_IMAGE_SIZE = 1024 * 1024;
  public static final String HYPHEN = "-";
  private static final String URL_IMAGE = "https://i.imgur.com/xCx7lQG.png";
  private static final String URL_IMAGE_1 = "https://i.imgur.com/xCx7lQ1.png";
  private static final String AUTO_UPLOAD_STRING = "auto-upload-data";
  private static String MAX_ROW_SIZE = "2";
  private static final String BRAND_CODE = "Att-Brand-Value-Code";
  private static final String BRAND = "brand";
  private static final String NAME = "name";
  private static final String ITEM_NAME_1 = "itemName1";
  private static final String ITEM_NAME_2 = "itemName2";
  private static final String VARIANT_IMAGE = "variant_image.jpeg";
  private static final String VARIANT_IMAGE_URL = "https://variant_image.png";
  private static final String VARIANT_IMAGE_AUTO_UPLOAD_URL = "https://variant_image_auto-upload.png";
  private static final String NOTES = "notes";


  private BulkCreateProductEventModel bulkCreateProductEventModel;
  private BulkProcessData bulkProcessData;
  private BulkProcessData bulkProcessData1;
  private BulkProcessData bulkProcessData2;

  private BulkProcessImage bulkProcessImage;
  private BulkProcessImage bulkProcessImage1;
  private BulkProcess bulkProcess;
  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigAutoUpload;
  private Map<String, String> protectedBrandNameCodeMap;
  private PickupPointResponse pickupPointResponse;
  private Map<String, Object> row1;
  private Map<String, Object> row2;

  private LinkedHashMap<Object, Object> getInputRowData(String fileName) throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName);
    TypeReference<LinkedHashMap<Object, Object>> typeRef = new TypeReference<LinkedHashMap<Object, Object>>() {
    };
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, typeRef);
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
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Objects.requireNonNull(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + excelFileName)))), "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Objects.requireNonNull(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + zipFile)))), "UTF-8");
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setNotes(CATEGORY_CODE);
    bulkProcess.setInternationalMerchant(false);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.name());
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedBy(CREATED_BY);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setUpdatedBy(UPDATED_BY);

    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfig.setValue("1");

    bulkCreateProductEventModel =
        BulkCreateProductEventModel.builder().parentProduct(PARENT_PRODUCT).bulkProcessCode(BULK_PROCESS_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID).build();
    bulkProcessData = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(10)
        .bulkRequestData(BULK_DATA).status(BulkProcessData.STATUS_PENDING).build();
    bulkProcessData1 = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(8)
        .bulkRequestData(BULK_DATA_1).status(BulkProcessData.STATUS_PENDING).build();
    bulkProcessData2 = BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(9)
        .bulkRequestData(BULK_DATA_2).status(BulkProcessData.STATUS_PENDING).build();
    bulkProcessImage = BulkProcessImage.builder().bulkProcessId(bulkProcess.getId()).errorMessage("").build();
    bulkProcessImage1 = BulkProcessImage.builder().bulkProcessId(bulkProcess.getId()).build();
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
    Mockito.when(bulkProcessImageService.findByBulkProcessCodeAndImageUrl(any(BulkProcess.class), anyList()))
        .thenReturn(Arrays.asList(bulkProcessImage, bulkProcessImage1));
    systemParameterConfigAutoUpload = new SystemParameterConfig();
    systemParameterConfigAutoUpload.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfigAutoUpload.setValue(AUTO_UPLOAD_STRING);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(systemParameterConfigAutoUpload);
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "maxStockLimit", 10000000L);
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "ppNameDelimiter", "||");
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.TRUE);

    protectedBrandNameCodeMap = new HashMap<>();
    protectedBrandNameCodeMap.put(BRAND, BRAND_CODE);
    Mockito.when(protectedBrandValidationService.fetchProtectedBrandNameCodeMap(STORE_ID)).thenReturn(protectedBrandNameCodeMap);
    Mockito.when(protectedBrandValidationService.validateProtectedBrand(Mockito.anyMap(),
      Mockito.any(BulkProcess.class),Mockito.anyMap())).thenReturn(true);
    Mockito.when(protectedBrandValidationService.validateProtectedBrandForCn(Mockito.anyMap(),
        Mockito.any(BulkProcess.class),Mockito.anyMap())).thenReturn(true);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "productBundlingMaxNumberOfSkus", 10);
    row1 = new HashMap<>();
    row1.put("Image-1", "image1.jpg");
    row1.put("Image-2", "https://image2.jpg");
    row1.put("Image-3", "https://auto-upload-image3.jpg");
    row1.put("Image-4", "image4.jpg");
    row1.put("Image-5", "https://image2.jpg");
    row1.put("Image-6", "");
    row1.put("Image-7", "");
    row1.put("Image-8", "");

    row2 = new HashMap<>();
    row2.put("Image-1", "image1.jpg");
    row2.put("Image-2", "https://image2.jpg");
    row2.put("Image-3", "https://auto-upload-image3.jpg");
    row2.put("Image-4", "image4.jpg");
    row2.put("Image-5", "https://image2.jpg");
    row2.put("Image-6", "");
    row2.put("Image-7", "");
    row2.put("Image-8", "");

    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "sizeChartValueTypeDelimiter", "-");
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "allowedImageExtensions",
        List.of("jpg", "jpeg", "png", "webp"));
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "eanUpcValidLength",
        Arrays.asList(5, 8, 12, 13, 14, 15));
  }

  private CategoryDetailAndShippingResponse getGenericCategoryDetailResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryDetailPCBResponse");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    return objectMapper.readValue(inputStream, new TypeReference<CategoryDetailAndShippingResponse>() {
    });
  }

  private CategoryDetailAndShippingResponse getGenericCategoryDetailWarnaDefiningResponse() throws Exception {
    InputStream inputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "genericCategoryDetailPCBDefiningWarnaResponse");
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
    attributeResponse.setMandatory(true);
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
    Mockito.verifyNoMoreInteractions(pickupPointService);
    ProcessorUtils.deleteFile(ProcessorUtils.DATA_BASE_DIR);
  }

  @Test
  public void processEvent() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEvent_withPickupPoints() throws Exception {
    bulkProcessData.setNotes(PICKUP_POINT_CODE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void getAttributesTest() throws Exception {
    CategoryDetailResponse category = new CategoryDetailResponse();
    category.setCategoryAttributes(Collections.singletonList(new CategoryAttributeResponse()));
    category.getCategoryAttributes().get(0).setAttribute(new AttributeResponse());
    category.getCategoryAttributes().get(0).getAttribute().setName(NAME);
    when(attributeRepository.findOne(eq(STORE_ID), any())).thenReturn(new AttributeResponse());
    bulkCategoryProcessorServiceBean.getAttributes(STORE_ID, category, true, NAME);
    verify(attributeRepository).findOne(eq(STORE_ID), any());
  }

  @Test
  public void getAttributesIgnoreBrandTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "fetchBrandIgnoreCaseInCreation",
      true);
    CategoryDetailResponse category = new CategoryDetailResponse();
    category.setCategoryAttributes(Collections.singletonList(new CategoryAttributeResponse()));
    category.getCategoryAttributes().get(0).setAttribute(new AttributeResponse());
    category.getCategoryAttributes().get(0).getAttribute().setName(Constant.BRAND);
    category.getCategoryAttributes().get(0).getAttribute().setId(STORE_ID);
    Mockito.when(
      pcbOutboundService.getAttributeDetailByIdIgnoreCase(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(getBrandAttributeResponse());
    bulkCategoryProcessorServiceBean.getAttributes(STORE_ID, category, true, NAME);
    Mockito.verify(pcbOutboundService)
      .getAttributeDetailByIdIgnoreCase(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void getAttributesBrandInputValueBlankTest() throws Exception {
    CategoryDetailResponse category = new CategoryDetailResponse();
    category.setCategoryAttributes(Collections.singletonList(new CategoryAttributeResponse()));
    category.getCategoryAttributes().get(0).setAttribute(new AttributeResponse());
    category.getCategoryAttributes().get(0).getAttribute().setName(Constant.BRAND);
    when(attributeRepository.findOne(eq(STORE_ID), any())).thenReturn(new AttributeResponse());
    bulkCategoryProcessorServiceBean.getAttributes(STORE_ID, category, true, StringUtils.EMPTY);
    verify(attributeRepository).findOne(eq(STORE_ID), any());
  }

  @Test
  public void processEventMppSwitchOn() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowDataMPPSwitchOn_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse=getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventMppSwitchOn_inReview() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      getInputRowData("generateInputRowDataMPPSwitchOn_Cn1"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
      bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse=getProfileResponse();
    profileResponse.getCompany().setCncActivated(false);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(any(), any())).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(any(), any(), any()))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID,
      inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(attributeRepository, times(3)).findOne(any(), any());
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
      .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
        TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
      TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventEmptyListTest() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(new ArrayList<>());
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
  }

  @Test
  public void processEventWithStock0Test() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Stok tersedia*", "0");
    hashMap.put("Stok minimum*", "0");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventNegativeStockTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Stok minimum*", "-10");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService,times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventNegativeStock1Test() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Stok tersedia*", "-10");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);  }

  @Test
  public void processEventNonParsableValuesTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Stok tersedia*", "ABCD");
    hashMap.put("Stok minimum*", "ABCD");
    hashMap.put("Normal Harga (Rp)*", "ABCD");
    hashMap.put("Harga Penjualan (Rp)*", "ABCD");
    hashMap.put("Berat (gram)*", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventNonParsableValues1Test() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Tipe Penanganan*", "Melalui partner logistik Blibli");
    hashMap.put("Tinggi (cm)*", "ABCD");
    hashMap.put("Panjang (cm)*", "ABCD");
    hashMap.put("Lebar (cm)*", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventDimensionLessThan1Test() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Tipe Penanganan*", "Melalui partner logistik Blibli");
    hashMap.put("Tinggi (cm)*", "-1");
    hashMap.put("Panjang (cm)*", "-1");
    hashMap.put("Lebar (cm)*", "-1");
    hashMap.put("Berat (gram)*", "-1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }


  @Test
  public void processEventEmptyPickupPointTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Kode Toko/Gudang*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventBpBopisInEligibleTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "bpBopisRestrictionEnabled",
      Boolean.TRUE);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.setBopisFlag(Boolean.FALSE);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventEmptyBrandTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Brand*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, ""))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository, times(2)).findOne(eq(STORE_ID), Mockito.anyString());
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void processEventWrongBrandTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Brand*", "ABCDEF");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, "ABCDEF"))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, "ABCDEF");
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventEmptyDescriptionTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Deskripsi*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventWrongFamilyColorTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Family Colour", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventWithEmptyFamilyColorTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Family Colour", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventEmptyDescriptiveWarnaTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventEmptyDescriptiveWarnaNonMandatoryTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    hashMap.put("Tipe Penanganan*", "Melalui partner logistik Blibli");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
  }

  @Test
  public void processEventEmptyDescriptiveWarnaNonMandatoryPureInstoreTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "instoreNewFlowEnabled", Boolean.TRUE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    hashMap.put("Tipe Penanganan*", "Melalui partner logistik Blibli");
    hashMap.put("Status/Pengiriman*", "0");
    hashMap.put(GenericBulkHeaders.INSTORE, "1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
  }

  @Test
  public void processEventEmptyDescriptiveWarnaNonMandatoryPureInstoreDimensionZeroTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "instoreNewFlowEnabled", Boolean.TRUE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    hashMap.put("Tipe Penanganan*", "Melalui partner logistik Blibli");
    hashMap.put("Status/Pengiriman*", "0");
    hashMap.put("Tinggi (cm)*", "0");
    hashMap.put(GenericBulkHeaders.INSTORE, "1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
  }

  @Test
  public void processEventEmptyDescriptiveWarnaNonMandatoryBopisTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
  }

  @Test
  public void processEventEmptyDefiningWarnaTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventDefiningWarnaTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            this.attributeRepository.addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(null);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(attributeRepository).addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventDefiningWarnaNonNullNewAttributeTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDefiningWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            this.attributeRepository.addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new AttributeValueResponse());
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(attributeRepository).addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventDefiningWarnaNonNullNewAttributeWarnaRemovedTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDefiningWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            this.attributeRepository.addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new AttributeValueResponse());
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWrongEANUPCTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventWrongEANUPCTest1() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "123456789");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventEmptyBuyableTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Bisa dibeli*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);  }

  @Test
  public void processEventWrongBuyableBlankTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Bisa dibeli*", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventMinPriceTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Normal Harga (Rp)*", "-1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processBulkEventTestException() throws Exception {
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(getInputRowData("generateInputRowData"));
    Mockito.doThrow(Exception.class).when(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
  }

  @Test
  public void processEventMinPriceTest1() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Harga Penjualan (Rp)*", "-1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventMinPriceSalePriceLessThanOfferPriceTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Harga Penjualan (Rp)*", "2639200000");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventInvalidTypeTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Tipe Penanganan*", "ABCD");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventEmptyNameTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Nama Produk*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventMandatoryAttributeMissingTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Fungsi*","");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventCreateProductExceptionTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.doThrow(Exception.class).when(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), Mockito.any());
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventCreateProductApplicationExceptionTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.doThrow(ApplicationException.class).when(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), Mockito.any());
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventCreateProductExceptionTest1() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameter).getMtaImageSource();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
        getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);

    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID,DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID,inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService)
      .trackProductCreationFailure(eq(bulkProcess.getBulkProcessCode()),
        Mockito.any(ProductCreationRequest.class), Mockito.anyString());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventCreateProductExceptionInstoreTest1() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "instoreNewFlowEnabled", true);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameter).getMtaImageSource();
    LinkedHashMap<Object, Object> rowData = getInputRowData("generateInputRowData_Cn");
    rowData.put(GenericBulkHeaders.INSTORE, "1");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(rowData);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService)
        .trackProductCreationFailure(eq(bulkProcess.getBulkProcessCode()), Mockito.any(ProductCreationRequest.class),
            Mockito.anyString());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventCreateProductExceptionNonBopisProductTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameter).getMtaImageSource();
    LinkedHashMap<Object, Object> excelRows = getInputRowData("generateInputRowData_Cn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(excelRows);
    excelRows.put(GenericBulkHeaders.PRODUCT_TYPE, BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService)
        .trackProductCreationFailure(eq(bulkProcess.getBulkProcessCode()), Mockito.any(ProductCreationRequest.class),
            Mockito.anyString());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventBpBopisTest1() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "bpBopisRestrictionEnabled",
      Boolean.TRUE);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Deskripsi*", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.setBopisFlag(Boolean.FALSE);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void processEventBpBopisTest2() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "bpBopisRestrictionEnabled",
      Boolean.TRUE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
      Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
      Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyBoolean())).thenReturn(true);
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameter).getMtaImageSource();
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
      bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
        bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
      BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
      AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.setBopisFlag(Boolean.FALSE);
    Mockito.when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);

    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
  }

  @Test
  public void generateBulkProcessDataAndImageTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<Object, Object> map = getInputRowData("generateInputRowData");
    List<Object> userInputRows = new ArrayList<>();
    List<Object> headers = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
      headers.add(entry.getKey());
    }
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
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
    bulkCategoryProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        headers, null);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
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
  public void generateBulkProcessDataAndImage_withAccessiblePickupPointsTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<Object, Object> map = getInputRowData("generateInputRowData");
    List<Object> userInputRows = new ArrayList<>();
    List<Object> headers = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
      headers.add(entry.getKey());
    }
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
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
    bulkCategoryProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        headers, PICKUP_POINT_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
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
  public void generateBulkProcessDataAndImageWithURLTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<Object, Object> map = getInputRowData("generateInputRowData");
    List<Object> userInputRows = new ArrayList<>();
    List<Object> headers = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      Object header = entry.getKey();
      if (header.toString().startsWith(CnExcelHeaderNames.FOTO_PREFIX)) {
        userInputRows.add(URL_IMAGE);
      } else {
        userInputRows.add(entry.getValue());
      }
      headers.add(entry.getKey());
    }
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig("A", "10", "B"));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
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
    bulkCategoryProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        headers, null);
    Mockito.verify(bulkProcessDataService).saveBulkProcessData(Mockito.anyList());
    Mockito.verify(bulkProcessImageService).saveBulkProcessImage(Mockito.anyList());
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
    verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    verify(bulkProcessService).publishBulkImageDownloadEventModel(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateBulkProcessDataAndImageWithURLEmptyParentTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<Object, Object> map = getInputRowData("generateInputRowData");
    List<Object> userInputRows = new ArrayList<>();
    List<Object> headers = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      Object header = entry.getKey();
      if (header.toString().startsWith(CnExcelHeaderNames.FOTO_PREFIX)) {
        userInputRows.add(URL_IMAGE);
      } else if (header.toString().startsWith(CnExcelHeaderNames.PARENT)) {
        userInputRows.add("");
      } else {
        userInputRows.add(entry.getValue());
      }
      headers.add(header);
    }
    headers.add(BulkParameters.WAREHOUSE_STOCK_HEADER);
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE))
        .thenReturn(new SystemParameterConfig("A", "10", "B"));
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
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
    bulkCategoryProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess, Arrays.asList(userInputRows),
        headers, null);
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
  public void processEventEmptyImageTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", "");
    hashMap.put("Foto-2", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventEmptyImageIsInternationalMerchantTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", "");
    hashMap.put("Foto-2", "");
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
        PickupPointFilterRequest.class));
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(bulkProcessData);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventImageURLTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put("Foto-2", URL_IMAGE);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setErrorMessage("Error");
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventMultipleRowsOldTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put("Foto-2", URL_IMAGE_1);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setErrorMessage("Error");
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(URL_IMAGE_1);
    bulkProcessImage1.setErrorMessage("Error");
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter, times(1)).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(pickupPointService, times(2))
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventMultipleRowsTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put("Foto-2", URL_IMAGE_1);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setErrorMessage("Error");
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(URL_IMAGE_1);
    bulkProcessImage1.setErrorMessage("Error");
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter, times(1)).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventMultipleRows1OldTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService,times(2)).getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(
      PickupPointFilterRequest.class));
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventMultipleRows1Test() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    when(objectMapper.readValue(eq(BULK_DATA_1), any(TypeReference.class))).thenReturn(getInputRowData("generateInputRowData_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventWithURLTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID,
        BUSINESS_PARTNER_CODE)).thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithURLVariantImageContainsInCommonImageTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    BulkProcessData bulkProcessData1 =
        BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(11)
            .bulkRequestData(BULK_DATA).status(BulkProcessData.STATUS_PENDING).build();
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    LinkedHashMap<Object, Object> hashMap1 = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    hashMap1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(VARIANT_IMAGE_URL);
    bulkProcessImage1.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap).thenReturn(hashMap1);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);

      bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
      Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
      Mockito.verify(bulkProcessDataService)
          .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
              BulkProcessData.STATUS_PENDING);
      Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
      Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
      Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
      Mockito.verify(pickupPointService, times(2))
          .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
      Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
      Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
      Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
      Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
      verify(systemParameter).getImageMaxSize();
      verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess,
          PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
      verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
      verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
      Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
      Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
      Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
      Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
      Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithURLVariantImageContainsInCommonImageOldFlowTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    BulkProcessData bulkProcessData1 =
        BulkProcessData.builder().bulkProcessId(bulkProcess.getId()).parentProduct(PARENT_PRODUCT).rowNumber(11)
            .bulkRequestData(BULK_DATA).status(BulkProcessData.STATUS_PENDING).build();
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    LinkedHashMap<Object, Object> hashMap1 = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    hashMap1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(VARIANT_IMAGE_URL);
    bulkProcessImage1.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap).thenReturn(hashMap1);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData, bulkProcessData1));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData1)).thenReturn(bulkProcessData1);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);

    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(4)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess,
        PARENT_PRODUCT, BulkProcessData.STATUS_PENDING);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithURLVariantImageTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }


  @Test
  public void processEventWithURLVariantImageURLBulkProcessImageNullTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithURLVariantImageURLBulkProcessImageNonNullTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(VARIANT_IMAGE_URL);
    bulkProcessImage1.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithURLVariantImageURLBulkProcessImageNonNullErrorMessageNotEmptyTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(VARIANT_IMAGE_URL);
    bulkProcessImage1.setLocation(BulkProcessImage.COLUMN_LOCATION);
    bulkProcessImage1.setErrorMessage(BulkProcessImage.COLUMN_ERROR_MESSAGE);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventWithURLVariantImageURLBulkProcessImageNonNullErrorMessageNotEmptyContainsCommonImageTest()
      throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "overridePickupPointInCreation",
      Boolean.TRUE);
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", VARIANT_IMAGE_URL);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setImageURL(VARIANT_IMAGE_URL);
    bulkProcessImage1.setLocation(BulkProcessImage.COLUMN_LOCATION);
    bulkProcessImage1.setErrorMessage(BulkProcessImage.COLUMN_ERROR_MESSAGE);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage, bulkProcessImage1));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
  }

  @Test
  public void processEventWithURLVariantImageSameAsCommonImageTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(false);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Foto-1*", URL_IMAGE);
    hashMap.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, BulkProcessImage.COLUMN_LOCATION);
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setImageURL(URL_IMAGE);
    bulkProcessImage.setLocation(BulkProcessImage.COLUMN_LOCATION);
    when(bulkProcessImageService.findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any())).thenReturn(
        Arrays.asList(bulkProcessImage));
    when(objectMapper.readValue(eq(BULK_DATA), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(
        bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
            bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID)).thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Mockito.anyString(), Mockito.any());
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).trackProductCreationFailure(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventCategoryAttributesMFDTest() throws Exception {
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    CategoryDetailAndShippingResponse categoryDetailAndShippingResponse = getGenericCategoryDetailResponse();
    for (CategoryAttributeResponse categoryAttribute : categoryDetailAndShippingResponse.getCategoryAttributes()) {
      if (!"Brand".equals(categoryAttribute.getAttribute().getName())) {
        categoryAttribute.setMarkForDelete(true);
      }
    }
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(categoryDetailAndShippingResponse);
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
            TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidEanUPCTest1() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "12345");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidEanUPCTest2() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "12345678");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidEanUPCTest3() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "123456789101");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidEanUPCTest4() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Model/EAN/UPC", "1234567891012");
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
  }

  @Test
  public void processEventValidBuyableValuesTest1() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Bisa dibeli*", BulkUploadOption.BUYABLE_FALSE.getdescription());
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidBuyableValuesTest2() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Bisa dibeli*", BulkUploadOption.BUYABLE_EN_FALSE.getdescription());
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventValidBuyableValuesTest3() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Bisa dibeli*", BulkUploadOption.BUYABLE_EN_TRUE.getdescription());
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
        .thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailResponse());
    Mockito
        .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
            .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(trackerService)
        .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void generateBulkProcessDataAndImageMaximumRowExceptionTest() throws Exception {
    systemParameterConfig.setValue(MAX_ROW_SIZE);
    LinkedHashMap<Object, Object> map = getInputRowData("generateInputRowData");
    List<Object> userInputRows = new ArrayList<>();
    List<Object> headers = new ArrayList<>();
    for (Map.Entry entry : map.entrySet()) {
      userInputRows.add(entry.getValue());
      headers.add(entry.getKey());
    }
    headers.add("Parent");
    headers.add("Parent");
    userInputRows.add("1234567890");
    userInputRows.add("12345678910");
    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(bulkProcessArgumentCaptor.capture())).thenReturn(bulkProcess);
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
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkCategoryProcessorServiceBean.generateBulkProcessDataAndImage(bulkProcess,
              Arrays.asList(userInputRows), headers, null));
    } finally {
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE);
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
  public void checkDuplicateVariantCreatingAttributeOldTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "variantAttributeSwitch", true);
    Mockito.when(fileStorageService
      .downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Mockito.when(
      businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
      .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(
      systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(3)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(3)).saveOperation(bulkProcessData);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService)
      .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService, times(2))
      .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "variantAttributeSwitch", true);
    Mockito.when(fileStorageService
        .downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
            Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
            Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
            .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
        .thenReturn(bulkProcess);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
        .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
        .thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(3)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(3)).saveOperation(bulkProcessData);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeInternationalOldFlowTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "resetCncFLagForNonCncPP", Boolean.FALSE);
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "variantAttributeSwitch", true);
    Mockito.when(fileStorageService
      .downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(),
        Mockito.anyInt(), Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING))
      .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(bulkProcessRepository
      .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
      businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
      .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(
      systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(3)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(3)).saveOperation(bulkProcessData);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService)
      .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService, times(2))
      .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeInternationalTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "variantAttributeSwitch", true);
    Mockito.when(fileStorageService
        .downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
            Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
            Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
            .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
        .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
        .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
        .thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(3)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(3)).saveOperation(bulkProcessData);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService)
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void checkDuplicateVariantCreatingAttributeInternationalNewTest() throws Exception {
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "variantAttributeSwitch", true);
    Mockito.when(fileStorageService
        .downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
            Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
            Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(),
            Mockito.anyInt(), Mockito.anyBoolean())).thenReturn(true);
    LinkedHashMap<Object, Object> hashMap = getInputRowData("generateInputRowData_Cn");
    hashMap.put("Warna", "");
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(hashMap);
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
            .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
                BulkProcessData.STATUS_PENDING))
        .thenReturn(Arrays.asList(bulkProcessData, bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
        .thenReturn(getGenericCategoryDetailWarnaDefiningResponse());
    Mockito.when(bulkProcessRepository
            .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
        .thenReturn(bulkProcess);
    ProfileResponse profileResponse = getProfileResponse();
    profileResponse.getCompany().setInternationalFlag(true);
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
        .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
        .thenReturn(getBrandAttributeResponse());
    AttributeResponse attributeResponse = getDescriptiveWarnaResponse();
    attributeResponse.setMandatory(false);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
        .thenReturn(attributeResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
        getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
            AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(
            systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(systemParameterConfig);
    Mockito.when(
            pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new ArrayList<>()).thenReturn(Collections.singletonList(pickupPointResponse));
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(3)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService)
        .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
            BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(3)).saveOperation(bulkProcessData);
    verify(objectMapper, times(2)).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageService)
        .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(bulkProcessService).saveOperation(Mockito.any(BulkProcess.class));
    Mockito.verify(pickupPointService, times(2))
        .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
  }

  @Test
  public void processEventDefaultTest() throws Exception {
    Mockito.when(fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
      Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
      Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class)))
      .thenReturn(getInputRowData("generateInputRowData_CnDuplicate"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString()))
      .thenReturn(getGenericCategoryDetailResponse());
    Mockito
      .when(bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class))).thenReturn(bulkProcess);
    Mockito.when(businessPartnerRepository
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID)).thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
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
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(any());
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService).getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(productRepository).generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    Mockito.verify(pbpOutboundServiceBean)
        .createNewProduct(Mockito.any(), Mockito.any(), productCreationRequestArgumentCaptor.capture());
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    verify(systemParameter).getImageMaxSize();
    verify(bulkProcessDataService).findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
      BulkProcessData.STATUS_PENDING);
    verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any(BulkProcessData.class));
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    /*Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);*/
    Mockito.verify(bulkProcessImageService).findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }


  @Test
  public void generateImageFilenameTest() {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setItemGeneratedName(ITEM_NAME_2);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put("https://image2.jpg", "image2.jpg");
    imageUrlMap.put("https://auto-upload-image3.jpg", "image3.jpg");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(
        new SystemParameterConfig(Constant.AUTO_UPLOAD_URL_SUBSTRING, "auto-upload",
            Constant.AUTO_UPLOAD_URL_SUBSTRING));
    bulkCategoryProcessorServiceBean.generateImageFilename(Arrays.asList(row1, row2), productCreationRequest,
        imageUrlMap);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void generateImageFilenameVariantImageNotContainsAutoUploadTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_AUTO_UPLOAD_URL);
    row2.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_AUTO_UPLOAD_URL);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setItemGeneratedName(ITEM_NAME_2);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put("https://image2.jpg", "image2.jpg");
    imageUrlMap.put("https://auto-upload-image3.jpg", "image3.jpg");
    imageUrlMap.put(VARIANT_IMAGE_AUTO_UPLOAD_URL, "image4.jpg");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(
        new SystemParameterConfig(Constant.AUTO_UPLOAD_URL_SUBSTRING, "auto-upload",
            Constant.AUTO_UPLOAD_URL_SUBSTRING));
    bulkCategoryProcessorServiceBean.generateImageFilename(Arrays.asList(row1, row2), productCreationRequest,
        imageUrlMap);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void generateImageFilenameVariantImageContainsAutoUploadTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row2.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setItemGeneratedName(ITEM_NAME_2);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put("https://image2.jpg", "image2.jpg");
    imageUrlMap.put("https://auto-upload-image3.jpg", "image3.jpg");
    imageUrlMap.put(VARIANT_IMAGE_URL, "image4.jpg");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(
        new SystemParameterConfig(Constant.AUTO_UPLOAD_URL_SUBSTRING, "auto-upload",
            Constant.AUTO_UPLOAD_URL_SUBSTRING));
    bulkCategoryProcessorServiceBean.generateImageFilename(Arrays.asList(row1, row2), productCreationRequest,
        imageUrlMap);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void generateImageFilenameVariantImageContainsAutoUploadImageUrlMapDoesNotContainTest() {
    row1 = new HashMap<>();
    row2 = new HashMap<>();
    row1.put(BulkCnCreationHeaderNames.IMAGE_ID_HEADER_NAME + "1", "https://auto-upload-image3.jpg");
    row2.put(BulkCnCreationHeaderNames.IMAGE_ID_HEADER_NAME + "1", "image3.jpg");
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row2.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setItemGeneratedName(ITEM_NAME_2);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put("https://auto-upload-image3.jpg", "image3.jpg");
    imageUrlMap.put(VARIANT_IMAGE_URL, "image4.jpg");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(
        new SystemParameterConfig(Constant.AUTO_UPLOAD_URL_SUBSTRING, "auto-upload",
            Constant.AUTO_UPLOAD_URL_SUBSTRING));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkCategoryProcessorServiceBean.generateImageFilename(Arrays.asList(row1, row2),
              productCreationRequest, imageUrlMap));
    } finally {
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    }
  }

  @Test
  public void generateImageFilenameVariantImageContainsAutoUploadGeneratedItemNameAndHttpTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row1.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, ITEM_NAME_1);
    row1.remove("Image-5");
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put("https://image2.jpg", "image2.jpg");
    imageUrlMap.put("https://auto-upload-image3.jpg", "image3.jpg");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(
        new SystemParameterConfig(Constant.AUTO_UPLOAD_URL_SUBSTRING, "auto-upload",
            Constant.AUTO_UPLOAD_URL_SUBSTRING));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkCategoryProcessorServiceBean.generateImageFilename(Arrays.asList(row1),
              productCreationRequest, imageUrlMap));
    } finally {
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
    }
  }

  @Test
  public void prepareProductItemCreationRequestTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row1.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, ITEM_NAME_1);
    row1.remove("Image-5");
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put(VARIANT_IMAGE_URL, VARIANT_IMAGE_AUTO_UPLOAD_URL);
    bulkCategoryProcessorServiceBean.prepareProductItemCreationRequest(Arrays.asList(row1), productCreationRequest,
        new HashMap<>(), imageUrlMap, true, new HashSet<>(), new HashSet<>());
    Assertions.assertNotNull(productCreationRequest.getProductItemRequests().get(0).getImages());
  }

  @Test
  public void prepareProductItemCreationRequestURLImageTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row1.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, ITEM_NAME_1);
    row1.remove("Image-5");
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put(VARIANT_IMAGE_URL, VARIANT_IMAGE_AUTO_UPLOAD_URL);
    Map<String, String> mappingImageUrl = new HashMap<>();
    mappingImageUrl.put(VARIANT_IMAGE_AUTO_UPLOAD_URL, VARIANT_IMAGE);
    bulkCategoryProcessorServiceBean.prepareProductItemCreationRequest(Arrays.asList(row1), productCreationRequest,
        imageUrlMap, mappingImageUrl, true, new HashSet<>(), new HashSet<>());
    Assertions.assertNotNull(productCreationRequest.getProductItemRequests().get(0).getImages());
  }

  @Test
  public void prepareProductItemCreationRequestURLImageCommonImageTest() {
    row1.put(BulkCnCreationHeaderNames.VARIANT_IMAGE_ID, VARIANT_IMAGE_URL);
    row1.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, ITEM_NAME_1);
    row1.remove("Image-5");
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductCode(PRODUCT_CODE);
    productCreationRequest.setBrand(BRAND);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setItemGeneratedName(ITEM_NAME_1);
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));
    Map<String, String> imageUrlMap = new HashMap<>();
    imageUrlMap.put(VARIANT_IMAGE_URL, VARIANT_IMAGE_AUTO_UPLOAD_URL);
    Map<String, String> mappingImageUrl = new HashMap<>();
    mappingImageUrl.put(VARIANT_IMAGE_AUTO_UPLOAD_URL, VARIANT_IMAGE);
    bulkCategoryProcessorServiceBean.prepareProductItemCreationRequest(Arrays.asList(row1), productCreationRequest,
        imageUrlMap, mappingImageUrl, true, new HashSet<>(), Set.of(VARIANT_IMAGE_AUTO_UPLOAD_URL));
    Assertions.assertNotNull(productCreationRequest.getProductItemRequests().get(0).getImages());
  }

  @Test
  public void processEventWithPpCodeOverrideTest() throws Exception {
    Page<PickupPointResponse> pageResponse =
      new PageImpl<>(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "overridePickupPointInCreation",
      true);
    Mockito.when(
      fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      getInputRowData("EmptyPpCode_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(
      bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess,
        PARENT_PRODUCT, BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(),
      anyString())).thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class))).thenReturn(new ArrayList<>());
    Mockito.when(
      pickupPointService.getSinglePickupPointSummaryFilter(Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class))).thenReturn(pageResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(
        Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
        productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService, times(2))
      .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(bulkProcessImageService)
      .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    Mockito.verify(productRepository)
      .generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy());
    verify(systemParameter).getImageMaxSize();
    Mockito.verify(systemParameter, times(2)).getMtaImageSource();
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, CATEGORY_CODE);
    Mockito.verify(trackerService)
      .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
        TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, CATEGORY_CODE);
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService, times(2)).uploadImageFilesToSourceLocation(Mockito.any(),
      Mockito.any(), Mockito.any());
    Mockito.verify(pickupPointService)
      .getSinglePickupPointSummaryFilter(Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING);
  }

  @Test
  public void processEventWithPpCodeOverrideIEligibleTest() throws Exception {
    Page<PickupPointResponse> pageResponse =
      new PageImpl<>(Arrays.asList(pickupPointResponse, pickupPointResponse));
    ReflectionTestUtils.setField(bulkCategoryProcessorServiceBean, "overridePickupPointInCreation",
      true);
    Mockito.when(
      fileStorageService.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
        Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
        Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
        Mockito.anyBoolean())).thenReturn(true);
    when(objectMapper.readValue(anyString(), any(TypeReference.class))).thenReturn(
      getInputRowData("EmptyPpCode_Cn"));
    Mockito.when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE))
      .thenReturn(bulkProcess);
    Mockito.when(
      bulkProcessDataService.findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess,
        PARENT_PRODUCT, BulkProcessData.STATUS_PENDING)).thenReturn(Arrays.asList(bulkProcessData));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(),
      anyString())).thenReturn(getGenericCategoryDetailResponse());
    Mockito.when(
      bulkProcessRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(STORE_ID,
        BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    Mockito.when(bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
      .thenReturn(bulkProcess);
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE))
      .thenReturn(getProfileResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FAMILY_COLOUR_ID))
      .thenReturn(getFamilyColourResponse());
    Mockito.when(pcbOutboundService.getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue))
      .thenReturn(getBrandAttributeResponse());
    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),
      Mockito.any(PickupPointFilterRequest.class))).thenReturn(new ArrayList<>());
    Mockito.when(
      pickupPointService.getSinglePickupPointSummaryFilter(Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class))).thenReturn(pageResponse);
    Mockito.when(attributeRepository.findOne(STORE_ID, DESCRIPTIVE_WARNA_ID))
      .thenReturn(getDescriptiveWarnaResponse());
    Mockito.when(attributeRepository.findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID)).thenReturn(
      getAttributeResponse(FUNGSI_ATTRIBUTE_ID, FUNGSI_ATTRIBUTE_CODE, FUNGSI, FUNGSI,
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name(), false, false));
    Mockito.when(this.generatorRepository.generateShippingWeight(
        Mockito.any(GenerateShippingWeightRequest.class)))
      .thenReturn(generateGenerateShippingWeightResponse());
    Mockito.when(
        productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()))
      .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
      .thenReturn(systemParameterConfig);
    bulkCategoryProcessorServiceBean.processEvent(bulkCreateProductEventModel);
    Mockito.verify(this.categoryRepository)
      .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(anyString(), anyString());
    Mockito.verify(bulkProcessDataService)
      .findByBulkProcessCodeAndParentProductAndStatus(STORE_ID, bulkProcess, PARENT_PRODUCT,
        BulkProcessData.STATUS_PENDING);
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeV2(STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(pickupPointService, times(2))
      .getPickupPointSummaryFilter(Mockito.anyInt(), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(attributeRepository).findOne(STORE_ID, FAMILY_COLOUR_ID);
    Mockito.verify(pcbOutboundService).getAttributeDetailById(STORE_ID, BRAND_ID, inputBrandValue);
    Mockito.verify(attributeRepository).findOne(STORE_ID, FUNGSI_ATTRIBUTE_ID);
    Mockito.verify(attributeRepository).findOne(STORE_ID, DESCRIPTIVE_WARNA_ID);
    Mockito.verify(bulkProcessImageService)
      .findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Mockito.verify(systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.MINIMUM_PRICE);
    verify(systemParameter).getImageMaxSize();
    verify(objectMapper).readValue(anyString(), any(TypeReference.class));
    verify(bulkProcessService, times(1)).saveOperation(any());
    Mockito.verify(bulkProcessDataService, times(2)).saveOperation(Mockito.any());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(pickupPointService)
      .getSinglePickupPointSummaryFilter(Mockito.anyInt(), Mockito.anyInt(),
        Mockito.any(PickupPointFilterRequest.class));
  }
}
