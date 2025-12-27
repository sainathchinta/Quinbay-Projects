package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BulkProcessRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.service.download.BulkFailedProductFileService;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
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
import org.mockito.verification.VerificationMode;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

public class ProductLevel3ProcessorServiceBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = "123";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLT-23808";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String UNIFIED_BULK = "Blibli mass upload template_v3.8.4.xlsm";
  private static final String BULK_UPLOAD_XLSX_FILE = "ProductLevel3.xlsx";
  private static final String BULK_UPLOAD_XLSM_FILE = "ProductLevel3.xlsm";
  private static final String DEFAULT_CATEGORY_CODE = "10317";
  private static final String DEFAULT_CATEGORY_CODE_1 = "54587";
  private static final String DEFAULT_CATEGORY_CODE_2 = "10221";
  private static final String DEFAULT_CATEGORY_CODE_3 = "10318";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_ATTRIBUTE_CODE = "WA-0000002";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_DIRECTORY_PATH = "dirpath";
  private static final VerificationMode AT_LEAST_ONE = times(1);
  private static final VerificationMode AT_LEAST_FOUR = times(4);
  private static final VerificationMode AT_LEAST_NONE = times(0);
  private static final String PICKUP_POINT_CODE = "PP-3000001";
  private static final String PRODUCT_LEVEL3_PROCESSOR_DIR = "ProductLevel3Processor";
  private static final String EMPTY_DEFINING_ATTRIBUTE_EXCEL_FILE_XLSX = "ProductLevel3ProcessorEmptyDefiningAttribute_1_cn.xlsx";
  private static final String EMPTY_DEFINING_ATTRIBUTE_EXCEL_FILE_ZIP = "ProductLevel3ProcessorEmptyDefiningAttribute_1.zip";
  private static final int MAX_IMAGE_SIZE = 1024 * 1024;
  private static final String AUTO_UPLOAD_STRING = "auto-upload-data";
  private static final String BULK_CREATE_PRIORITY_EVENT = "com.gdn.mta.bulk.create.priority";
  private static final String BULK_CREATE_EVENT = "com.gdn.mta.bulk.create";
  private static final String BULK_CONVERTED_FILE_CREATE_EVENT = "com.gdn.mta.bulk.converted.file.product.create";
  private static final String BULK_GENERIC_CREATE_EVENT = "com.gdn.mta.bulk.generic.create";
  private static final String BULK_GENERIC_CREATE_PRIORITY_EVENT = "com.gdn.mta.bulk.generic.create.priority";

  @Mock
  private BulkProcessRepository bulkProcessRepository;

  @Mock
  private TrackerService trackerService;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private GeneratorRepository generatorRepository;

  @Mock
  private NotificationService notificationService;

  @Mock
  private BulkFailedProductFileService bulkFailedProductFileService;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkCategoryProcessorService bulkCategoryProcessorService;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ProductLevel3ProcessorServiceBean productLevel3ProcessorServiceBean;

  @Captor
  private ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<List<Object>>> invalidRowsArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private GdnRestListResponse<ConfigurationStatusResponse> configurationStatusResponseGdnRestListResponse;

  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigAutoUpload;
  private PickupPointResponse pickupPointResponse;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.when(systemParameter.getImageMaxSize()).thenReturn(MAX_IMAGE_SIZE);

    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR);
    GenerateShippingWeightResponse generateShippingWeightResponse = this.generateGenerateShippingWeightResponse();
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class))).thenReturn(generateShippingWeightResponse);

    Mockito.when(systemParameter.getMtaImageSource()).thenReturn(Thread.currentThread().getContextClassLoader().getResource(PRODUCT_LEVEL3_PROCESSOR_DIR).getPath());
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfig.setValue("1");

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constant.MINIMUM_PRICE)).thenReturn(systemParameterConfig);
    Mockito.when(fileStorageServiceBean.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
      Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
      Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyBoolean())).thenReturn(true);
    systemParameterConfigAutoUpload = new SystemParameterConfig();
    systemParameterConfigAutoUpload.setVariable(Constant.MINIMUM_PRICE);
    systemParameterConfigAutoUpload.setValue(AUTO_UPLOAD_STRING);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        Constant.AUTO_UPLOAD_URL_SUBSTRING)).thenReturn(systemParameterConfigAutoUpload);
    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.attributeRepository, this.categoryRepository, this.trackerService,
        this.businessPartnerRepository, this.productRepository, this.productBusinessPartnerRepository,
        this.bulkProcessRepository, this.generatorRepository,
        this.bulkFailedProductFileService, kafkaProducer, this.bulkProcessService, this.pcbOutboundService,
        this.systemParameterConfigService, this.bulkCategoryProcessorService, this.kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
    ProcessorUtils.deleteFile(ProcessorUtils.DATA_BASE_DIR);
  }

  private Map<String, String> getArgs() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    return args;
  }

  private Map<String, String> getXlsmArgs() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSM_FILE);
    return args;
  }

  private Map<String, String> getUnifiedArgs() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.UNIFIED_BULK);
    return args;
  }

  private Map<String, String> getArgs_1() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE_1);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    return args;
  }

  private Map<String, String> getArgs_2() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE_2);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    return args;
  }

  private Map<String, String> getArgs_3() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE_3);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    return args;
  }

  private Map<String, String> getArgs_4() throws Exception {
    Map<String, String> args = new HashMap<String, String>();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    return args;
  }

  public AttributeRepository getAttributeRepository() {
    return this.attributeRepository;
  }

  private Map<String, AttributeResponse> getAttributes() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "attributes");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    List<AttributeResponse> content =
        objectMapper.readValue(inputStream, new TypeReference<List<AttributeResponse>>() {});
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (AttributeResponse attribute : content) {
      attributes.put(attribute.getId(), attribute);
    }
    return attributes;
  }

  private Map<String, AttributeResponse> getAttributes_1() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "attributes_1");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    List<AttributeResponse> content =
        objectMapper.readValue(inputStream, new TypeReference<List<AttributeResponse>>() {});
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (AttributeResponse attribute : content) {
      attributes.put(attribute.getId(), attribute);
    }
    return attributes;
  }

  private Map<String, AttributeResponse> getAttributes_2() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "attributes_2");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    List<AttributeResponse> content =
        objectMapper.readValue(inputStream, new TypeReference<List<AttributeResponse>>() {});
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (AttributeResponse attribute : content) {
      attributes.put(attribute.getId(), attribute);
    }
    return attributes;
  }

  private Map<String, AttributeResponse> getAttributes_3() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "attributes_4");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    List<AttributeResponse> content =
        objectMapper.readValue(inputStream, new TypeReference<List<AttributeResponse>>() {});
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (AttributeResponse attribute : content) {
      attributes.put(attribute.getId(), attribute);
    }
    return attributes;
  }

  private BulkProcess getBulkProcess() throws Exception {
    Date date = Calendar.getInstance().getTime();
    BulkProcess bulkProcess =
        new BulkProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>());
    bulkProcess.setStoreId(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID);
    bulkProcess.setCreatedBy(ProductLevel3ProcessorServiceBeanTest.DEFAULT_USERNAME);
    bulkProcess.setCreatedDate(date);
    bulkProcess.setUpdatedBy(ProductLevel3ProcessorServiceBeanTest.DEFAULT_USERNAME);
    bulkProcess.setUpdatedDate(date);
    return bulkProcess;
  }

  private BulkProcessQueue getBulkProcessQueue() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getArgs());
    return bulkProcessQueue;
  }

  private BulkProcessQueue getXlsmBulkProcessQueue() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getXlsmArgs());
    return bulkProcessQueue;
  }


  private BulkProcessQueue getBulkProcessQueue_1() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getArgs_1());
    return bulkProcessQueue;
  }

  private BulkProcessQueue getBulkProcessQueue_2() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getArgs_2());
    return bulkProcessQueue;
  }

  private BulkProcessQueue getBulkProcessQueue_3() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getArgs_3());
    return bulkProcessQueue;
  }

  private BulkProcessQueue getBulkProcessQueue_4() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_CODE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE, this.getArgs_4());
    return bulkProcessQueue;
  }

  public BulkProcessRepository getBulkProcessRepository() {
    return this.bulkProcessRepository;
  }

  private ProfileResponse getBusinessPartner() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "businessPartner");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    ProfileResponse businessPartner =
        objectMapper.readValue(inputStream, new TypeReference<ProfileResponse>() {});
    return businessPartner;
  }

  private ProfileResponse getInternationalBusinessPartner() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "internationalBusinessPartner");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    ProfileResponse businessPartner =
        objectMapper.readValue(inputStream, new TypeReference<ProfileResponse>() {});
    return businessPartner;
  }

  public BusinessPartnerRepository getBusinessPartnerRepository() {
    return this.businessPartnerRepository;
  }

  private CategoryDetailResponse getCategory() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "category");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    CategoryDetailResponse category =
        objectMapper.readValue(inputStream, new TypeReference<CategoryDetailResponse>() {});
    return category;
  }

  private CategoryDetailResponse getCategory_1() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "category_1");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    CategoryDetailResponse category =
        objectMapper.readValue(inputStream, new TypeReference<CategoryDetailResponse>() {});
    return category;
  }

  private CategoryDetailResponse getCategory_2() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "category_2");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    CategoryDetailResponse category =
        objectMapper.readValue(inputStream, new TypeReference<CategoryDetailResponse>() {});
    return category;
  }

  private CategoryDetailResponse getCategory_4() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "category_4");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    CategoryDetailResponse category =
        objectMapper.readValue(inputStream, new TypeReference<CategoryDetailResponse>() {});
    return category;
  }

  public CategoryRepository getCategoryRepository() {
    return this.categoryRepository;
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithoutZip() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.xlsx"))),
            "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  private Map<String, String> getUnifiedFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Blibli mass upload template_v3.8.4.xlsm"))),
            "UTF-8");
    files.put("xlsm", excelData);
    return files;
  }

  private Map<String, String> getGenericFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorGeneric.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_1() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_1.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_1.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }


  private Map<String, String> getFiles_EmptyDefiningAttribute() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + EMPTY_DEFINING_ATTRIBUTE_EXCEL_FILE_XLSX))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + EMPTY_DEFINING_ATTRIBUTE_EXCEL_FILE_ZIP))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_2() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_2.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_2.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_3() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_3.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_3.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_ForISMerchantWithWarranty() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_4.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_4.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelAttributes() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelAttributes.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelAttributes_2() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelAttributes_2.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelAttributes_1() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelAttributes_1.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_1.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelAttributes_5(String fileName) throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader().getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + fileName))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_1.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelDescriptiveAttributes() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelDescriptiveAttributes.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelHeaders_1() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelHeaders_1.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelHeaders_2() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelHeaders_2.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelImages() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelImages.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelProductInformation() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelProductInformation.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelProductInformation_1() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelProductInformation_1.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelSkuValueAttributes_2() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator
                    + "ProductLevel3ProcessorWithInvalidExcelSkuValueAttributes_2.xlsx"))), "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_2.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelImageType() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorWithInvalidExcelImageType.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_3.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidExcelImageContent() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_9.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_3_invalid_image_content.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidProductName() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_InvalidProductName.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_3.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithUSPMoreThan400Characters() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_USP_Exceeded_Limit.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithEmptyBrand() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorBrandEmpty.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidBrand() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorInvalidBrand.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithInvalidBrands() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorInvalidBrand-100.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithDeliveryStatus() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorInvalidBrand-100-Delivery.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithBundlingColumn() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "final-cn-sheet-bundling.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;}

  private Map<String, String> getFilesWithDescriptionMoreThan5000Characters() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_Description_Exceeded_Limit.xlsx"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))), "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFilesWithIgnoredHeader() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3ProcessorWithIgnoredHeader.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_WithImageSizeMoreThan4Mb() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_9.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_image_size.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_WithImageResolutionLessThan600Pixels() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_9.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_less_resolution_image.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_WithInvalidEAN() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_invalid_ean.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_image_size.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  private Map<String, String> getFiles_WithInvalidEANMultipleProduts() throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_invalid_ean_multi.xlsx"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor_image_size.zip"))),
            "UTF-8");
    files.put("xlsx", excelData);
    files.put("zip_1", zipData);
    return files;
  }

  public ProductBusinessPartnerRepository getProductBusinessPartnerRepository() {
    return this.productBusinessPartnerRepository;
  }

  private ProductDetailResponse getProductDetail() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "productDetail");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    ProductDetailResponse productDetail =
        objectMapper.readValue(inputStream, new TypeReference<ProductDetailResponse>() {});
    return productDetail;
  }

  private ProductDetailResponse getProductDetail_1() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "productDetail_1");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    ProductDetailResponse productDetail =
        objectMapper.readValue(inputStream, new TypeReference<ProductDetailResponse>() {});
    return productDetail;
  }

  private ProductDetailResponse getProductDetail_2() throws Exception {
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "productDetail_2");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    ProductDetailResponse productDetail =
        objectMapper.readValue(inputStream, new TypeReference<ProductDetailResponse>() {});
    return productDetail;
  }

  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse() throws Exception {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(1D);
    return generateShippingWeightResponse;
  }

  private ConfigurationStatusResponse getConfigurationStatusResponse() {
    ConfigurationStatusResponse configurationStatusResponse = ConfigurationStatusResponse.builder().reviewConfig("Post-live").build();
    return configurationStatusResponse;
  }

  public ProductLevel3ProcessorServiceBean getProductLevel3ProcessorServiceBean() {
    return this.productLevel3ProcessorServiceBean;
  }

  public ProductRepository getProductRepository() {
    return this.productRepository;
  }

  @Test
  public void testPreProcess() throws Exception {
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode",DEFAULT_CATEGORY_CODE_3);

    Map<String, String> files1 = new HashMap<String, String>();
    String excelData =
      new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
        .currentThread()
        .getContextClassLoader()
        .getResourceAsStream(
          PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
        "UTF-8");
    String zipData =
      new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
        .currentThread()
        .getContextClassLoader()
        .getResourceAsStream(
          PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
        "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR
      + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode()
        + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
      excelFile);
    ProcessorUtils.decompressFile(
      ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);



    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
      ".xlsx");

    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().build());
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));

    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());

    File dataBaseDirectory = new File(ProcessorUtils.DATA_BASE_DIR);
    for (String bulkProcessCodeDirectoryPath : dataBaseDirectory.list()) {
      File bulkProcessCodeDirectory = new File(dataBaseDirectory, bulkProcessCodeDirectoryPath);
    }
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkCreateEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkCreateEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcess_withXlsmFile() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "bopisCncRestrictionEnabled", true);
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSM_FILE);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode",DEFAULT_CATEGORY_CODE_3);

    Map<String, String> files1 = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR
        + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode()
            + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
        excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + ProcessorUtils.DATA_RAW_DIR, zipFile);



    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
        ".xlsm");

    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));

    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());

    File dataBaseDirectory = new File(ProcessorUtils.DATA_BASE_DIR);
    for (String bulkProcessCodeDirectoryPath : dataBaseDirectory.list()) {
      File bulkProcessCodeDirectory = new File(dataBaseDirectory, bulkProcessCodeDirectoryPath);
    }
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkGenericCreateEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkGenericCreateEvent();
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcess_withXlsmFile2() throws Exception {
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSM_FILE);
    Mockito.when(kafkaTopicProperties.getBulkGenericCreatePriorityEvent()).thenReturn(BULK_GENERIC_CREATE_PRIORITY_EVENT);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode",DEFAULT_CATEGORY_CODE_3);

    Map<String, String> files1 = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR
        + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode()
            + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
        excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + ProcessorUtils.DATA_RAW_DIR, zipFile);



    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
        ".xlsm");

    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().trustedSeller(true).build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));

    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());


    File dataBaseDirectory = new File(ProcessorUtils.DATA_BASE_DIR);
    for (String bulkProcessCodeDirectoryPath : dataBaseDirectory.list()) {
      File bulkProcessCodeDirectory = new File(dataBaseDirectory, bulkProcessCodeDirectoryPath);
    }
    Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkGenericCreatePriorityEvent()),
        Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkGenericCreatePriorityEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcessWithoutzZipfile() throws Exception {
    Map<String, String> files = this.getFilesWithoutZip();
    Map<String, String> args = this.getArgs();
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
      ".xlsx");
    InputStream fileInputStream = new FileInputStream(new File(
      ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx"));
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());

    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkCreateEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkCreateEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcess_withXlsmFile3() throws Exception {
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    args.put("categoryCode", ProductLevel3ProcessorServiceBeanTest.DEFAULT_CATEGORY_CODE);
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSM_FILE);
    Mockito.when(kafkaTopicProperties.getBulkGenericCreateEvent()).thenReturn(BULK_GENERIC_CREATE_EVENT);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode",DEFAULT_CATEGORY_CODE_3);

    Map<String, String> files1 = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR
        + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode()
            + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
        excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + ProcessorUtils.DATA_RAW_DIR, zipFile);



    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
        ".xlsm");

    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));

    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());

    File dataBaseDirectory = new File(ProcessorUtils.DATA_BASE_DIR);
    for (String bulkProcessCodeDirectoryPath : dataBaseDirectory.list()) {
      File bulkProcessCodeDirectory = new File(dataBaseDirectory, bulkProcessCodeDirectoryPath);
    }
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkGenericCreateEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkGenericCreateEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcessWithoutzZipfile3() throws Exception {
    Map<String, String> files = this.getFilesWithoutZip();
    Map<String, String> args = this.getArgs();
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
        ".xlsx");
    InputStream fileInputStream = new FileInputStream(new File(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx"));
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().trustedSeller(true).build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkCreateEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkCreateEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcessWithoutzZipfile2() throws Exception {
    Map<String, String> files = this.getFilesWithoutZip();
    Map<String, String> args = this.getArgs();
    Mockito.when(kafkaTopicProperties.getBulkCreatePriorityEvent()).thenReturn(BULK_CREATE_PRIORITY_EVENT);
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode +
        ".xlsx");
    InputStream fileInputStream = new FileInputStream(new File(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx"));
    Mockito.when(
            businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().trustedSeller(true).build());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    this.productLevel3ProcessorServiceBean
        .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
            "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());

    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkCreatePriorityEvent()), Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkCreatePriorityEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void testPreProcessWithoutzZipfileForIncorrectFileTest() throws Exception {
    ApiIncorrectInputDataException apiIncorrectInputDataException = new ApiIncorrectInputDataException();
    try {
      Map<String, String> files = this.getFilesWithoutZip1();
      Map<String, String> args = this.getArgs();
      Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any())).thenReturn(null);
      String bulkProcessCode = UUID.randomUUID().toString();
      mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx");
      InputStream fileInputStream = new FileInputStream(
          new File(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx"));
      Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
          .thenReturn(ProfileResponse.builder().build());
      Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
          new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
              SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
      this.productLevel3ProcessorServiceBean.preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode,
          "");
    }
    catch (ApiIncorrectInputDataException e)
    {
      apiIncorrectInputDataException = e;
    }finally {
      Assertions.assertEquals("Invalid excel file",apiIncorrectInputDataException.getErrorMessage());
    }
  }

  private Map<String, String> getFilesWithoutZip1() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    try (Workbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("Sheet1");
      Row row = sheet.createRow(0);
      Cell cell = row.createCell(0);
      cell.setCellValue("something");
      String excelData = workbookToBase64(workbook);
      files.put("xlsx", excelData);
    }
    return files;
  }

  private String workbookToBase64(Workbook workbook) throws IOException {
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      workbook.write(outputStream);
      byte[] bytes = outputStream.toByteArray();
      return new String(Base64.encodeBase64(bytes), "UTF-8");
    }
  }

  @Test
  public void testPreProcessWithInvalidExcelFile() throws Exception {
    Map<String, String> files = this.getFiles();
    files.put("xlsx", "");
    Map<String, String> args = this.getArgs();
    try {
      this.productLevel3ProcessorServiceBean
          .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_REQUEST_ID,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, UUID.randomUUID().toString(),
              "");
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.getBulkProcessRepository(),
          ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).saveAndFlush(
          (BulkProcess) Mockito.any());
    }
  }

  @Test
  public void testPreProcessWithInvalidZipFile() throws Exception {
    Map<String, String> files = this.getFiles();
    files.put("zip_1", "");
    Map<String, String> args = this.getArgs();
    try {
      this.productLevel3ProcessorServiceBean
          .preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_REQUEST_ID,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, UUID.randomUUID().toString(),
              "");
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(
          ProductLevel3ProcessorServiceBean.ZIP_FILE_MUST_NOT_BE_BLANK));
      Mockito.verify(this.getBulkProcessRepository(),
          ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).saveAndFlush(
          (BulkProcess) Mockito.any());
    }
  }

  @Disabled
  @Test
  public void testProcess() throws Exception {
    Mockito.when(fileStorageServiceBean.downloadAndValidateProductCreationImages(Mockito.any(BulkProcess.class),
      Mockito.any(BulkUploadErrorCounter.class), Mockito.anyMap(),
      Mockito.any(StringBuilder.class), Mockito.anyList(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyBoolean())).thenReturn(true);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode",DEFAULT_CATEGORY_CODE_3);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = new HashMap<String, String>();
    String excelData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
            "UTF-8");
    String zipData =
        new String(Base64.encodeBase64(IOUtils.toByteArray(Thread
            .currentThread()
            .getContextClassLoader()
            .getResourceAsStream(
                PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
            "UTF-8");
    files.put("xlsm", excelData);
    files.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR
        + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode()
            + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
        excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    InputStream inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "category_3");
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    CategoryDetailResponse category =
        objectMapper.readValue(inputStream, new TypeReference<CategoryDetailResponse>() {});
    ProfileResponse businessPartner = this.getBusinessPartner();
    inputStream =
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "attributes_3");
    List<AttributeResponse> content =
        objectMapper.readValue(inputStream, new TypeReference<List<AttributeResponse>>() {});
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (AttributeResponse attribute : content) {
      attributes.put(attribute.getId(), attribute);
    }
    AttributeValueResponse attributeValueResponse = new AttributeValueResponse();
    attributeValueResponse.setValue(ATTRIBUTE_VALUE);
    attributeValueResponse.setAllowedAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    Mockito.when(categoryRepository
        .filterCategoryHierarchyByCategoryCode(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString()))
        .thenReturn(getCategoryResponsesWithNullActivationInterval());
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(
        this.getCategoryRepository().findByStoreIdAndCategoryCodeAndMarkDeleteFalse(
            bulkProcessQueue.getStoreId(), categoryCode)).thenReturn(category);
    Mockito.when(
        this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(
                bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode())).thenReturn(
        businessPartner);
    for (Map.Entry<String, AttributeResponse> entry : attributes.entrySet()) {
      Mockito.when(
          this.attributeRepository.findOne(
              ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
              String.valueOf(entry.getKey()))).thenReturn(entry.getValue());
    }
    Mockito.when(productRepository.generateProductCode(Mockito.any(), Mockito.anyString()))
        .thenReturn(new GdnRestSimpleResponse(DEFAULT_REQUEST_ID, DEFAULT_PRODUCT_CODE));

    Mockito.when(this.bulkFailedProductFileService
        .createFile(any(), any(BulkProcess.class), any(), eq(BulkParameters.DATA_SHEET),
            eq(true)))
        .thenReturn(DEFAULT_DIRECTORY_PATH);
    Mockito.when(this.bulkFailedProductFileService.getDownloadLinkHtml(DEFAULT_DIRECTORY_PATH))
        .thenReturn(DEFAULT_DIRECTORY_PATH);
    Mockito
        .when(this.attributeRepository.addNewAttribute(Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(attributeValueResponse);
    Mockito.when(this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        bulkProcessQueue.getBulkProcessCode(),
        bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
        Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService,
        ProductLevel3ProcessorServiceBeanTest.AT_LEAST_FOUR).saveOperation(bulkProcess);
    Mockito
        .verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(),
        ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(),
            bulkProcess.getBusinessPartnerCode());
    for (Map.Entry<String, AttributeResponse> entry : attributes.entrySet()) {
      Mockito.verify(this.attributeRepository,
          ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findOne(
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, String.valueOf(entry.getKey()));
    }
    Mockito.verify(this.generatorRepository).generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class));
    Mockito.verify(this.productRepository).generateProductCode(Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productRepository, ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .createProduct(Mockito.any(), Mockito.any(),Mockito.any());
    Mockito.verify(trackerService, times(2)).sendTracker(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(this.bulkFailedProductFileService)
        .createFile(any(), any(BulkProcess.class), any(), eq(BulkParameters.DATA_SHEET),
            eq(true));
    Mockito.verify(this.bulkFailedProductFileService).getDownloadLinkHtml(DEFAULT_DIRECTORY_PATH);
    Mockito.verify(categoryRepository)
        .filterCategoryHierarchyByCategoryCode(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString());
    Mockito.verify(this.pcbOutboundService).getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        bulkProcessQueue.getBulkProcessCode(),
        bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
        Arrays.asList(configurationStatusRequest));
    Mockito.verify(this.systemParameterConfigService, times(3)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    Assertions.assertEquals(1, bulkProcess.getBulkProcessNotes().size());
    Assertions.assertNotNull(bulkProcess.getStartDate());
    Assertions.assertNotNull(bulkProcess.getEndDate());
  }

  @Test
  public void testProcessWithInvalidBulkProcessCode() throws Exception {
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    BulkProcess bulkProcess = this.getBulkProcess();
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(null);
    try {
      this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(applicationException.getErrorCodes(), ErrorCategory.DATA_NOT_FOUND);
        Mockito.verify(this.getBulkProcessRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
            .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
                bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
        Mockito.verify(this.getCategoryRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE)
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(),
                categoryCode);
        Mockito.verify(this.getBusinessPartnerRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE)
            .filterByBusinessPartnerCodeV2(
                bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
        Mockito.verify(this.getAttributeRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).findOne(
            Mockito.eq(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID),
            Mockito.anyString());
        Mockito.verify(this.getProductRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).create(
            (CreateProductRequest) Mockito.any());
        Mockito.verify(this.getProductRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE)
            .findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
        Mockito.verify(this.getProductRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).update(
            (ProductRequest) Mockito.any());
        Mockito.verify(this.getBulkProcessRepository(),
            ProductLevel3ProcessorServiceBeanTest.AT_LEAST_NONE).save(
            (BulkProcess) Mockito.any());
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  private List<CategoryResponse> getCategoryResponses() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    categoryResponse1.setInternalActivationInterval(24);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId("id");
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

  private List<CategoryResponse> getCategoryResponsesWithNullActivationInterval() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId("id");
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse = new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(), DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithDeliveryStatus();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository().findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode)).thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository().filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode())).thenReturn(businessPartner);
    Mockito.when(this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, bulkProcessQueue.getBulkProcessCode(), bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
    Mockito.verify(bulkCategoryProcessorService).generateBulkProcessDataAndImage(Mockito.any(BulkProcess.class), Mockito.anyList(), Mockito.anyList(),
        eq(null));
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnBundlingOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithBundlingColumn();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
      this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
      Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
              bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
      Mockito.verify(this.bulkProcessService, times(3)).saveOperation(bulkProcess);
      Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
      Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
      Mockito.verify(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), any());
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnBundlingOnCCMerchantTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithBundlingColumn();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setMerchantType("CC");
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    businessPartner.getCompany().setInternationalFlag(true);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(3)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), any());
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOffBundlingOnCCMerchantTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEnabled", false);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithBundlingColumn();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setMerchantType("CC");
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    bulkProcess.setInternationalMerchant(true);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    businessPartner.getCompany().setInternationalFlag(true);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(3)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), any());
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnCncTrueTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse = new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(), DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorInvalidBrand-100-CNC.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository().findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode)).thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository().filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode())).thenReturn(businessPartner);
    Mockito.when(this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, bulkProcessQueue.getBulkProcessCode(), bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
    Mockito.verify(bulkCategoryProcessorService).generateBulkProcessDataAndImage(Mockito.any(BulkProcess.class), Mockito.anyList(), Mockito.anyList(),
        eq(null));
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnCncTrueIsTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse = new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(), DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorPassForISMerchant_cn-Delivery.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository().findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode)).thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository().filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode())).thenReturn(businessPartner);
    Mockito.when(this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, bulkProcessQueue.getBulkProcessCode(), bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
    Mockito.verify(bulkCategoryProcessorService).generateBulkProcessDataAndImage(Mockito.any(BulkProcess.class), Mockito.anyList(), Mockito.anyList(),
        eq(null));
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnCncTrueIsInstoreTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "instoreNewFlowEnabled", true);
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse = new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(), DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorPassForISMerchantInstore.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.getCompany().setOfflineToOnlineFlag(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository().findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository().findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode)).thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository().filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode())).thenReturn(businessPartner);
    Mockito.when(this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, bulkProcessQueue.getBulkProcessCode(), bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(), bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(2)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE).filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
    Mockito.verify(bulkCategoryProcessorService).generateBulkProcessDataAndImage(Mockito.any(BulkProcess.class), Mockito.anyList(), Mockito.anyList(),
        eq(null));
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnCncTrueIsTest2() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "validateBulkMaxNumberOfRows", true);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "bulkMaxNumberOfRows", 1);
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files =
        this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorPassForISMerchant_cn-Delivery.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    try {
      this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
              bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
      Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
      Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
      Mockito.verify(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), any());
      Mockito.verify(bulkProcessService, times(3)).saveOperation(Mockito.any());
    }
  }

  @Test
  public void testProcessWithBulkSwitchTrueMPPSwitchOnCncTrueIsTest3() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "validateBulkMaxNumberOfRows", true);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "bulkMaxNumberOfRows", 6);
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files =
        this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorPassForISMerchant_cn-Delivery.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    try {
      this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
              bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
      Mockito.verify(this.bulkProcessService, times(2)).saveOperation(bulkProcess);
      Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
      Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
          .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
      Mockito.verify(systemParameterConfigService)
          .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_SWITCH);
      Mockito.verify(bulkCategoryProcessorService)
          .generateBulkProcessDataAndImage(Mockito.any(BulkProcess.class), Mockito.anyList(), Mockito.anyList(), Mockito.isNull());
    }
  }

  @Test
  public void processCategoryProductCreationEmptyFileTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "productBundlingEligibleMerchantTypes", "CC");
    BulkProcessQueue bulkProcessQueue = this.getBulkProcessQueue();
    ConfigurationStatusRequest configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(bulkProcessQueue.getArgs().get("categoryCode")).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(getConfigurationStatusResponse()), new PageMetaData(),
            DEFAULT_REQUEST_ID);
    BulkProcess bulkProcess = this.getBulkProcess();
    Map<String, String> files = this.getFilesWithInvalidExcelAttributes_5("ProductLevel3ProcessorEmptyFile.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    byte[] zipFile = Base64.decodeBase64(files.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String categoryCode = bulkProcessQueue.getArgs().get("categoryCode");
    CategoryDetailResponse category = this.getCategory();
    ProfileResponse businessPartner = this.getBusinessPartner();
    businessPartner.getCompany().setCncActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    Map<String, AttributeResponse> attributes = this.getAttributes();
    Mockito.when(this.getBulkProcessRepository()
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING)).thenReturn(bulkProcess);
    Mockito.when(this.bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);
    Mockito.when(this.getCategoryRepository()
            .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode))
        .thenReturn(category);
    Mockito.when(this.getBusinessPartnerRepository()
            .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode()))
        .thenReturn(businessPartner);
    Mockito.when(
        this.pcbOutboundService.getconfigurationstatus(DEFAULT_STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkProcessQueue.getBulkProcessCode(),
            bulkProcessQueue.getArgs().get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER),
            Arrays.asList(configurationStatusRequest))).thenReturn(configurationStatusResponseGdnRestListResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        SystemParameterConfigNames.BULK_SWITCH)).thenReturn(new SystemParameterConfig("A", "true", "B"));
    Mockito.when(fileStorageServiceBean.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
        .thenReturn(excelFile);
    this.getProductLevel3ProcessorServiceBean().process(bulkProcessQueue);
    Mockito.verify(this.getBulkProcessRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(bulkProcessQueue.getStoreId(),
            bulkProcessQueue.getBulkProcessCode(), BulkProcess.STATUS_PENDING);
    Mockito.verify(this.bulkProcessService, times(3)).saveOperation(bulkProcess);
    Mockito.verify(this.getCategoryRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcessQueue.getStoreId(), categoryCode);
    Mockito.verify(this.getBusinessPartnerRepository(), ProductLevel3ProcessorServiceBeanTest.AT_LEAST_ONE)
        .filterByBusinessPartnerCodeV2(bulkProcessQueue.getStoreId(), bulkProcess.getBusinessPartnerCode());
    Mockito.verify(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), any());
  }

  private void mockFile(String filePath) throws Exception {
    Map<String, String> files = this.getFilesForCateogry();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, excelFile);
  }

  private Map<String, String> getFilesForCateogry() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader().getResourceAsStream(
        PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  @Test
  public void testPreProcessSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean,
        "avoidRedundantDownloadInBulkCreation", true);
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    List<String> list = new ArrayList<>();
    list.add("ProductLevel3Processor.xlsx");
    list.add("ProductLevel3Processor.zip");
    args.put(Constant.FILE_NAMES, String.join(Constant.UNICODE_DELIMITER, list));
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any()))
        .thenReturn(null);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode", DEFAULT_CATEGORY_CODE_3);
    Map<String, String> files1 = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm"))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream(
            PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip"))),
        "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL,
        excelFile);
    ProcessorUtils.decompressFile(
        ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
            + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode
        + ".xlsx");
    Mockito.when(
            fileStorageServiceBean.getListOfMultipartFile(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(bytesToMultipartFile(IOUtils.toByteArray(
                Thread.currentThread().getContextClassLoader().getResourceAsStream(
                    PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.xlsx")),
            "sample.xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")));
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().build());
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    this.productLevel3ProcessorServiceBean.preProcess(
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args,
        bulkProcessCode, "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean, times(2))
        .getListOfMultipartFile(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkCreateEvent()),
        Mockito.any(BulkProcessQueue.class));
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Mockito.verify(kafkaTopicProperties, times(3)).getBulkCreateEvent();
    Assertions.assertEquals(BulkProcess.STATUS_PENDING,
        bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void preProcess_Converted_File_Upload() throws Exception {
    Mockito.when(kafkaTopicProperties.getBulkConvertedProductCreationEvent())
        .thenReturn(BULK_CONVERTED_FILE_CREATE_EVENT);
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean, "avoidRedundantDownloadInBulkCreation", true);
    Map<String, String> files = this.getFiles();
    Map<String, String> args = this.getArgs();
    args.put(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue(), "true");
    List<String> list = new ArrayList<>();
    list.add("ProductLevel3Processor.xlsm");
    list.add("ProductLevel3Processor.zip");
    args.put(Constant.FILE_NAMES, String.join(Constant.UNICODE_DELIMITER, list));
    Mockito.when(this.getBulkProcessRepository().saveAndFlush((BulkProcess) Mockito.any())).thenReturn(null);
    BulkProcessQueue bulkProcessQueue = this.getXlsmBulkProcessQueue();
    bulkProcessQueue.getArgs().put("categoryCode", DEFAULT_CATEGORY_CODE_3);
    Map<String, String> files1 = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Objects.requireNonNull(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "Android_template_new_cn.xlsm")))),
        "UTF-8");
    String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(Objects.requireNonNull(
        Thread.currentThread().getContextClassLoader()
            .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.zip")))),
        "UTF-8");
    files1.put("xlsm", excelData);
    files1.put("zip_1", zipData);
    byte[] excelFile = Base64.decodeBase64(files1.get("xlsm"));
    byte[] zipFile = Base64.decodeBase64(files1.get("zip_1"));
    ProcessorUtils.createDirectories(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode());
    ProcessorUtils.createFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + bulkProcessQueue.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSM_EXCEL, excelFile);
    ProcessorUtils.decompressFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessQueue.getBulkProcessCode() + File.separator
        + ProcessorUtils.DATA_RAW_DIR, zipFile);
    String bulkProcessCode = UUID.randomUUID().toString();
    mockFile(ProcessorUtils.DATA_BASE_DIR + bulkProcessCode + Constant.SLASH + bulkProcessCode + ".xlsx");
    Mockito.when(fileStorageServiceBean.getListOfMultipartFile(Mockito.anyString(), Mockito.anyList())).thenReturn(
        Collections.singletonList(bytesToMultipartFile(IOUtils.toByteArray(Objects.requireNonNull(
                Thread.currentThread().getContextClassLoader()
                    .getResourceAsStream(PRODUCT_LEVEL3_PROCESSOR_DIR + File.separator + "ProductLevel3Processor.xlsx"))),
            "sample.xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")));
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(ProfileResponse.builder().build());
    Mockito.when(kafkaTopicProperties.getBulkCreateEvent()).thenReturn(BULK_CREATE_EVENT);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(
            ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID, SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED))
        .thenReturn(new SystemParameterConfig(SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    this.productLevel3ProcessorServiceBean.preProcess(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
        ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args, bulkProcessCode, "");
    Mockito.verify(bulkProcessService).saveBulkProcess(bulkProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean, times(2)).getListOfMultipartFile(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(businessPartnerRepository)
        .filterByBusinessPartnerCodeV2(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED);
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcessArgumentCaptor.getValue().getStatus());
    Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkConvertedProductCreationEvent()),
        Mockito.any(BulkProcessQueue.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkConvertedProductCreationEvent();
  }

  @Test
  public void testPreProcessSwitchOnXlsxFileNotFound() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean,
        "avoidRedundantDownloadInBulkCreation", true);
    Map<String, String> files = new HashMap<>();
    Map<String, String> args = new HashMap<>();
    List<String> list = new ArrayList<>();
    list.add("ProductLevel3Processor.xlsx");
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    args.put(Constant.FILE_NAMES, String.join(Constant.UNICODE_DELIMITER, list));
    String bulkProcessCode = UUID.randomUUID().toString();
    Exception exception = Assertions.assertThrows(FileNotFoundException.class, () -> {
      productLevel3ProcessorServiceBean.preProcess(
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args,
          bulkProcessCode, "");
    });
    Assertions.assertTrue(
        exception.getMessage().contains(ProductLevel3ProcessorServiceBean.EXCEL_FILE_NOT_FOUND));
    Mockito.verify(fileStorageServiceBean, times(1))
        .getListOfMultipartFile(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testPreProcessSwitchOnXlsFileNotFound() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean,
        "avoidRedundantDownloadInBulkCreation", true);
    Map<String, String> files = new HashMap<>();
    Map<String, String> args = new HashMap<>();
    List<String> list = new ArrayList<>();
    list.add("ProductLevel3Processor.xls");
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    args.put(Constant.FILE_NAMES, String.join(Constant.UNICODE_DELIMITER, list));
    String bulkProcessCode = UUID.randomUUID().toString();
    Exception exception = Assertions.assertThrows(FileNotFoundException.class, () -> {
      productLevel3ProcessorServiceBean.preProcess(
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args,
          bulkProcessCode, "");
    });
    Assertions.assertTrue(
        exception.getMessage().contains(ProductLevel3ProcessorServiceBean.EXCEL_FILE_NOT_FOUND));
    Mockito.verify(fileStorageServiceBean, times(1))
        .getListOfMultipartFile(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testPreProcessSwitchOnXlsmFileNotFound() throws Exception {
    ReflectionTestUtils.setField(productLevel3ProcessorServiceBean,
        "avoidRedundantDownloadInBulkCreation", true);
    Map<String, String> files = new HashMap<>();
    Map<String, String> args = new HashMap<>();
    List<String> list = new ArrayList<>();
    list.add("ProductLevel3Processor.xlsm");
    args.put("excelFilename", ProductLevel3ProcessorServiceBeanTest.BULK_UPLOAD_XLSX_FILE);
    args.put(Constant.FILE_NAMES, String.join(Constant.UNICODE_DELIMITER, list));
    String bulkProcessCode = UUID.randomUUID().toString();
    Exception exception = Assertions.assertThrows(FileNotFoundException.class, () -> {
      productLevel3ProcessorServiceBean.preProcess(
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_STORE_ID,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BULK_PROCESS_TYPE,
          ProductLevel3ProcessorServiceBeanTest.DEFAULT_BUSINESS_PARTNER_CODE, files, args,
          bulkProcessCode, "");
    });
    Assertions.assertTrue(
        exception.getMessage().contains(ProductLevel3ProcessorServiceBean.EXCEL_FILE_NOT_FOUND));
    Mockito.verify(fileStorageServiceBean, times(1))
        .getListOfMultipartFile(Mockito.anyString(), Mockito.anyList());
  }

  public MultipartFile bytesToMultipartFile(byte[] bytes, String fileName, String contentType)
      throws IOException {
    return new MockMultipartFile(fileName, fileName, contentType, new ByteArrayInputStream(bytes));
  }
}
