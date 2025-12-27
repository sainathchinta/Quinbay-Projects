package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.models.MtaApiBaseQueueResult;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.rest.web.request.ImageApiRequest;
import com.gdn.x.mta.rest.web.request.ProductApiGeneralAttributeRequest;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ProductLevel3ApiProcessorServiceBeanTest {
  
  private final static String DEFAULT_REQUEST_ID = "a2sd1f3asd1f2as1d3212asdf";
  private final static String DEFAULT_STORE_ID = "10001";
  private final static String STORE_ID = "10001";
  private final static String DEFAULT_USERNAME = "username";
  private final static String BUSINESS_PARTNER_CODE = "TOT-15014";
  private final static String PRODUCT_CODE = "MTA-1234";
  private final static String CATEGORY_CODE = "AC-1000001";
  private final static String CREATE_PRODUCT_EVENT = "com.gdn.mta.create.product";

  private ProductApiRequest productApiRequest = new ProductApiRequest();
  private CompanyDTO companyDTO = new CompanyDTO();
  private ProfileResponse profileResponse = new ProfileResponse();
  
  @InjectMocks
  private ProductLevel3ApiProcessorServiceBean productLevel3ApiProcessorServiceBean;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private BusinessPartnerRepository businessPartnerRepository;
  @Mock
  private ProductRepository productRepository;
  @Mock
  private CategoryRepository categoryRepository;
  @Mock
  private AttributeRepository attributeRepository;
  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;
  @Mock
  private TrackerService trackerService;
  @Mock
  private GeneratorRepository generatorRepository;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    productApiRequest.setCategoryCode(ProductLevel3ApiProcessorServiceBeanTest.CATEGORY_CODE);
    productApiRequest.setTipePenanganan(1);
    productApiRequest.setMerchantCode("dummy-merchant-code");
    productApiRequest.setWeight(1D);
    companyDTO.setInternationalFlag(true);
    profileResponse.setCompany(companyDTO);
    GenerateShippingWeightResponse generateShippingWeightResponse = this.generateGenerateShippingWeightResponse();
    Mockito.when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(generateShippingWeightResponse);
    Mockito.when(this.categoryRepository.validateIsCnCategory(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(true);
    
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductDetailResponse productDetailResponse = this.productDetailResponseBuilder();
    productDetailResponse.getProductCategoryResponses().clear();
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(), 
        (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
          .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any())).thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString())).thenReturn(productDetailResponse);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    Mockito.doNothing().when(trackerService).sendTracker(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties);
  }
  
  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse() throws Exception {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(1D);
    return generateShippingWeightResponse;
  }

  @Test
  public void preProcessTest() throws Exception {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    Mockito.when(kafkaTopicProperties.getCreateProductEvent()).thenReturn(CREATE_PRODUCT_EVENT);
    productLevel3ApiProcessorServiceBean.preProcess(productApiRequest, null);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getCreateProductEvent()), Mockito.any(ProductApiRequest.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getCreateProductEvent();
  }
  
  @Test
  public void processFailedTest() throws Exception {
    Exception exception = new Exception("just error");
    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(
            Mockito.anyString(), Mockito.anyString())).thenThrow(exception);
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkProcessImageResponseEvent()),
        Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithEmptyProductCategoriesExceptionTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductDetailResponse productDetailResponse = this.productDetailResponseBuilder();
    productDetailResponse.getProductCategoryResponses().clear();

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }

  
  @Test
  public void process_UnknownBrand_FailedResponse() throws Exception {
    ProductApiRequest apiRequest = productApiRequestBuilder();
    for(ProductApiGeneralAttributeRequest feature : apiRequest.getFeatures()) {
      if(feature.getName().equals("Brand")){
        feature.setValue("UNKOWN-BRAND");
      }
    }
    productLevel3ApiProcessorServiceBean.process(apiRequest);   
    Mockito.verify(this.categoryRepository).validateIsCnCategory(Mockito.anyString(), Mockito.isNull());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void process_NotLastChildCategory_FailedResponse() throws Exception {
    Mockito.when(this.categoryRepository.validateIsCnCategory(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(false);
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());   
    Mockito.verify(this.categoryRepository).validateIsCnCategory(Mockito.anyString(), Mockito.isNull());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void process_UnknownCategory_FailedResponse() throws Exception {
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(null);
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());   
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processSuccessTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void process_WithZeroStock_Valid() throws Exception {
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    ProductApiRequest request = productApiRequestBuilder();
    request.setStock(0);
    productLevel3ApiProcessorServiceBean.process(request);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();

  }
  
  @Test
  public void process_CreateMasterFailed_ResponseFailed() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    
    when(productRepository.create((CreateProductRequest) Mockito.any()))
      .thenThrow(Exception.class);
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void process_CreateVariantFailed_ResponseFailed() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    
    when(productRepository.create((CreateProductRequest) Mockito.any()))
      .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    
    Mockito.doThrow(Exception.class).when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void process_CreateProductBPFailed_ResponseFailed() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(), 
        (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
          .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
      .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
      .thenReturn(productBusinessPartnerResponse);
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(), 
        eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    Mockito.when(productBusinessPartnerRepository.saveWithActivatedFalseReturnId(Mockito.any()))
      .thenThrow(Exception.class);
    
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq(kafkaTopicProperties.getBulkProcessImageResponseEvent()),
        Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processSuccessTestWIthNullUpc() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    ProductApiRequest request = productApiRequestBuilder();
    request.setUpcCode(null);
    request.setTipePenanganan(Constant.PRODUCT_TYPE_REGULAR);
    
    productLevel3ApiProcessorServiceBean.process(request);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()),Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithNullDescriptiveAttr() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilderWithNullDescriptive());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithUnknowBrand() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(
            Mockito.anyString(), Mockito.anyString())).thenReturn(bp);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new ArrayList<PredefinedAllowedAttributeValueResponse>());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    verify(businessPartnerRepository, Mockito.atLeast(1)).filterByBusinessPartnerCodeV2(
            Mockito.anyString(), Mockito.isNull());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processErrorPredefinedAllowed() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        businessPartnerRepository.filterByBusinessPartnerCodeV2(
            Mockito.anyString(), Mockito.anyString())).thenReturn(bp);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenThrow(Exception.class);
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    verify(businessPartnerRepository, Mockito.atLeast(1)).filterByBusinessPartnerCodeV2(
            Mockito.anyString(), Mockito.isNull());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithInvalidProductTypeRegularWeightThresholdExceptionTest() throws Exception {
    GenerateShippingWeightResponse generateShippingWeightResposne = new GenerateShippingWeightResponse();
    generateShippingWeightResposne.setShippingWeight(Constant.PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD + 1);
    when(this.generatorRepository.generateShippingWeight(Mockito.any(GenerateShippingWeightRequest.class))).thenReturn(
        generateShippingWeightResposne);
    ProductApiRequest productApiRequest = this.productApiRequestBuilder();
    productApiRequest.setTipePenanganan(Constant.PRODUCT_TYPE_REGULAR);
    productApiRequest.setWeight(Constant.PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD);
    this.productLevel3ApiProcessorServiceBean.process(productApiRequest);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithInvalidCategoryCodeTest() throws Exception {
    ProductApiRequest productApiRequest = this.productApiRequestBuilder();
    productApiRequest.setCategoryCode(null);
    this.productLevel3ApiProcessorServiceBean.process(productApiRequest);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processWithInvalidCategory() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenThrow(Exception.class);
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
    .thenReturn(productBusinessPartnerResponse);

    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    ProductApiRequest request = productApiRequestBuilder();
    request.setUpcCode(null);
    
    productLevel3ApiProcessorServiceBean.process(request);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    verify(categoryRepository, Mockito.times(1)).findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString());
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  
  
  @Test
  public void processSuccessFeatureAllowedValueNotFoundTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    ProductBusinessPartnerResponse productBusinessPartnerResponse =
        new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setGdnProductSku("gdn-sku");

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any())).thenReturn(
        PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDetailResponseBuilder());
    when(productBusinessPartnerRepository.getProductBusinessPartnerById(Mockito.anyString()))
        .thenReturn(productBusinessPartnerResponse);
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestAllowedValueNotFoundBuilder());
    Mockito.verify(productBusinessPartnerRepository,Mockito.times(0)).saveWithActivatedFalseReturnId(
        (ProductBusinessPartnerRequest) Mockito.any());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processFailedAttributeFeatureIsNotFoundTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestFeatureNotExistsBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processFailedAttributeVariasiIsNotFoundTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestVarianNotExistsBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processFailedAttributeFeatureNotCompleteTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    productLevel3ApiProcessorServiceBean.process(productApiRequestFeatureNotCompleteBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processFailedAttributeVarianNotCompleteTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
      .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestVarianNotCompleteBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processFailedAttributeFeaturesNotCompleteMfdTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());

    productLevel3ApiProcessorServiceBean.process(productApiRequestVarianNotCompleteBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processSuccessNotAllCategoryTest() throws Exception {
    ProfileResponse bp = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName(DEFAULT_USERNAME);
    bp.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bp.setCompany(company);
    bp.setAllCategory(false);

    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(categoryDetailResponseBuilder());
    when(
        attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(Mockito.anyString(),
            (GdnRestListRequest) Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PredefinedAllowedAttributeValueResponseBuilder());
    when(attributeRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(
        attributeResponseDefiningBuilder());
    when(productRepository.create((CreateProductRequest) Mockito.any()))
    .thenReturn(PRODUCT_CODE);
    when(productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDetailResponseBuilder());
    when(
        categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Mockito.anyString(),
            eq(CATEGORY_CODE))).thenReturn(categoryDetailResponseBuilder());
    Mockito.doNothing().when(productRepository).update((ProductRequest) Mockito.any());
    
    productLevel3ApiProcessorServiceBean.process(productApiRequestBuilder());
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }

  @Test
  public void processProductMerchantInternational() throws Exception {
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(
            eq(STORE_ID), eq(productApiRequest.getMerchantCode()))).thenReturn(profileResponse);

    productLevel3ApiProcessorServiceBean.process(productApiRequest);

    verify(businessPartnerRepository, times(1))
            .filterByBusinessPartnerCodeV2(eq(STORE_ID),
                    eq(productApiRequest.getMerchantCode()));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(MtaApiBaseQueueResult.class));
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }

  private ProductDetailResponse productDetailResponseBuilder() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId("product-request-id");
    productDetailResponse.setVersion(0L);
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setName("product-name");
    productDetailResponse.setLength(1D);
    productDetailResponse.setWidth(1D);
    productDetailResponse.setHeight(1D);
    productDetailResponse.setWeight(1D);
    productDetailResponse.setDescription("desc".getBytes());
    productDetailResponse.setLongDescription("desc".getBytes());
    productDetailResponse.setBrand("MAC");
    productDetailResponse.setUniqueSellingPoint("unique");
    productDetailResponse.setUom("PC");
    productDetailResponse.setProductStory("product-story");
    productDetailResponse.setSpecificationDetail("specification-detail");
    productDetailResponse.setUrl("url");
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.getProductCategoryResponses().add(productCategoryResponseBuilder());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.getProductAttributeResponses().addAll(productAttributeResponseBuilder());
    productDetailResponse.setProductItemResponses(productItemResponseBuilder());
    productDetailResponse.setImages(new ArrayList<Image>());
    Image image = new Image(false, "image.png", 0);
    productDetailResponse.getImages().add(image);
    return productDetailResponse;
  }
  
  private Set<ProductItemResponse> productItemResponseBuilder() {
    Set<ProductItemResponse> productItemResponses = new HashSet<ProductItemResponse>();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId("product-item-id");
    productItemResponses.add(productItemResponse);
    return productItemResponses;
  }
  
  private List<ProductAttributeResponse> productAttributeResponseBuilder() {
    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<ProductAttributeResponse>();    
    //build predefined response
    ProductAttributeResponse productAttributePredefinedResponse = new ProductAttributeResponse();
    productAttributePredefinedResponse.setAttribute(attributeResponsePredefinedBuilder());
    productAttributePredefinedResponse.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributePredefinedValueResponse = new ProductAttributeValueResponse();
    productAttributePredefinedValueResponse.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueResponse());
    productAttributePredefinedResponse.getProductAttributeValues().add(productAttributePredefinedValueResponse);
    
    // build descriptive response
    ProductAttributeResponse productAttributeDescriptiveResponse = new ProductAttributeResponse();
    productAttributeDescriptiveResponse.setAttribute(attributeResponseDescriptiveBuilder());
    productAttributeDescriptiveResponse.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributeDescriptiveValueResponse = new ProductAttributeValueResponse();
    productAttributeDescriptiveResponse.getProductAttributeValues().add(productAttributeDescriptiveValueResponse);

    // build defining response
    ProductAttributeResponse productAttributeDefiningResponse = new ProductAttributeResponse();
    productAttributeDefiningResponse.setAttribute(attributeResponseDefiningBuilder());
    productAttributeDefiningResponse.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    ProductAttributeValueResponse productAttributeDefiningValueResponse = new ProductAttributeValueResponse();
    productAttributeDefiningValueResponse.setAllowedAttributeValue(new AllowedAttributeValueResponse());
    productAttributeDefiningResponse.getProductAttributeValues().add(productAttributeDefiningValueResponse);
    
    productAttributeResponses.add(productAttributePredefinedResponse);
    productAttributeResponses.add(productAttributeDescriptiveResponse);
    productAttributeResponses.add(productAttributeDefiningResponse);
    return productAttributeResponses;
  }
  
  private ProductCategoryResponse productCategoryResponseBuilder() {
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    productCategoryResponse.setCategory(categoryResponse);
    return productCategoryResponse;
  }
  
  private List<PredefinedAllowedAttributeValueResponse> PredefinedAllowedAttributeValueResponseBuilder() {
    List<PredefinedAllowedAttributeValueResponse> predefinedList = new ArrayList<PredefinedAllowedAttributeValueResponse>(); 
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse1 = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse1.setValue("Nike");
    predefinedList.add(predefinedAllowedAttributeValueResponse1);
    
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("MAC");
    predefinedList.add(predefinedAllowedAttributeValueResponse);
    
    return predefinedList;
  }
  
  private CategoryDetailResponse categoryDetailResponseBuilder() {
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCatalog(new CatalogResponse());
    CategoryAttributeResponse categoryAttributePredefined = new CategoryAttributeResponse();
    categoryAttributePredefined.setMarkForDelete(false);
    categoryAttributePredefined.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined.setAttribute(attributeResponsePredefinedBuilder());
    
    CategoryAttributeResponse categoryAttributePredefined2 = new CategoryAttributeResponse();
    categoryAttributePredefined2.setMarkForDelete(false);
    categoryAttributePredefined2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined2.setAttribute(attributeResponsePredefinedMFDBuilder());
    
    CategoryAttributeResponse categoryAttributePredefined3 = new CategoryAttributeResponse();
    categoryAttributePredefined3.setMarkForDelete(true);
    categoryAttributePredefined3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined3.setAttribute(attributeResponsePredefinedBuilder());
    
    CategoryAttributeResponse categoryAttributePredefined4 = new CategoryAttributeResponse();
    categoryAttributePredefined4.setMarkForDelete(false);
    categoryAttributePredefined4.setStoreId(DEFAULT_STORE_ID);
    categoryAttributePredefined4.setAttribute(attributeResponsePredefinedBuilderGaransi());
    
    CategoryAttributeResponse categoryAttributeDescriptive = new CategoryAttributeResponse();
    categoryAttributeDescriptive.setMarkForDelete(false);
    categoryAttributeDescriptive.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDescriptive.setAttribute(attributeResponseDescriptiveBuilder());
    
    CategoryAttributeResponse categoryAttributeDefining = new CategoryAttributeResponse();
    categoryAttributeDefining.setMarkForDelete(false);
    categoryAttributeDefining.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining.setAttribute(attributeResponseDefiningBuilder());
    
    CategoryAttributeResponse categoryAttributeDefining2 = new CategoryAttributeResponse();
    categoryAttributeDefining2.setMarkForDelete(false);
    categoryAttributeDefining2.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining2.setAttribute(attributeResponseDefiningMFDBuilder());
    
    CategoryAttributeResponse categoryAttributeDefining3 = new CategoryAttributeResponse();
    categoryAttributeDefining3.setMarkForDelete(true);
    categoryAttributeDefining3.setStoreId(DEFAULT_STORE_ID);
    categoryAttributeDefining3.setAttribute(attributeResponseDefiningBuilder());

    categoryDetailResponse.setCategoryAttributes(new ArrayList<CategoryAttributeResponse>());
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined3);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributePredefined4);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDescriptive);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining2);
    categoryDetailResponse.getCategoryAttributes().add(categoryAttributeDefining3);
    
    return categoryDetailResponse;
  }
  
  private AttributeResponse attributeResponsePredefinedBuilderGaransi() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id-garansi");
    attributeResponse.setName("Garansi");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }
  
  private AttributeResponse attributeResponsePredefinedBuilder() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id");
    attributeResponse.setName("Brand");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }
  
  private AttributeResponse attributeResponsePredefinedMFDBuilder() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-predefined-id1");
    attributeResponse.setName("x1");
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setMarkForDelete(true);
    return attributeResponse;
  }
  
  private AttributeResponse attributeResponseDescriptiveBuilder() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-descriptive-id");
    attributeResponse.setName("Resolusi Layar");
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    return attributeResponse;
  }
  
  private AttributeResponse attributeResponseDefiningBuilder() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-defining-id");
    attributeResponse.setName("Warna");
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueResponse>());
    attributeResponse.getAllowedAttributeValues().add(allowedAttributeValueResponseBuilder());
    return attributeResponse;
  }
  
  private AttributeResponse attributeResponseDefiningMFDBuilder() {
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attr-defining-id1");
    attributeResponse.setName("Warna");
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attributeResponse.setSkuValue(false);
    attributeResponse.setMarkForDelete(true);
    attributeResponse.setAllowedAttributeValues(new ArrayList<AllowedAttributeValueResponse>());
    attributeResponse.getAllowedAttributeValues().add(allowedAttributeValueResponseBuilder());
    return attributeResponse;
  }
  
  private AllowedAttributeValueResponse allowedAttributeValueResponseBuilder() {
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setId("allowed-attr-id");
    allowedAttributeValueResponse.setValue("Putih");
    return allowedAttributeValueResponse;
  }
  
  private ProductApiRequest productApiRequestBuilderWithNullDescriptive() {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    productApiRequest.setProductName("product-name");
    productApiRequest.setLength(1D);
    productApiRequest.setCategoryCode(CATEGORY_CODE);
    productApiRequest.setWidth(1D);
    productApiRequest.setHeight(1D);
    productApiRequest.setWeight(1D);
    productApiRequest.setDesc("desc");
    productApiRequest.setUniqueSellingPoint("unique");
    productApiRequest.setProductStory("product-story");
    productApiRequest.setUrl("url");
    productApiRequest.setUpcCode("upc-code");
    ProductApiGeneralAttributeRequest featureBrand = new ProductApiGeneralAttributeRequest("Brand", "MAC");
    
    ProductApiGeneralAttributeRequest variasi = new ProductApiGeneralAttributeRequest("Warna", "PutihNotFound");
    List<ProductApiGeneralAttributeRequest> featureList = new ArrayList<ProductApiGeneralAttributeRequest>();
    List<ProductApiGeneralAttributeRequest> variasiList = new ArrayList<ProductApiGeneralAttributeRequest>();
    variasiList.add(variasi);
    
    ProductApiGeneralAttributeRequest featureDescriptive1 = new ProductApiGeneralAttributeRequest("Resolusi Layar", null);
    ProductApiGeneralAttributeRequest featurePredefinedOther = new ProductApiGeneralAttributeRequest("Garansi", "MAC");
    featureList.add(featureBrand);
    featureList.add(featureDescriptive1);
    featureList.add(featurePredefinedOther);
    
    productApiRequest.setVariasi(variasiList);
    productApiRequest.setFeatures(featureList);
    ImageApiRequest imageApiRequest = new ImageApiRequest();
    imageApiRequest.setLocationPath("image.png");
    imageApiRequest.setSequence(0);
    List<ImageApiRequest> imageApiRequestList = new ArrayList<ImageApiRequest>();
    imageApiRequestList.add(imageApiRequest);
    productApiRequest.setImages(imageApiRequestList);
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestBuilder() {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    productApiRequest.setProductName("product-name");
    productApiRequest.setLength(1D);
    productApiRequest.setCategoryCode(CATEGORY_CODE);
    productApiRequest.setWidth(1D);
    productApiRequest.setHeight(1D);
    productApiRequest.setWeight(1D);
    productApiRequest.setDesc("desc");
    productApiRequest.setUniqueSellingPoint("unique");
    productApiRequest.setProductStory("product-story");
    productApiRequest.setUrl("url");
    productApiRequest.setUpcCode("upc-code");
    productApiRequest.setStock(10);
    ProductApiGeneralAttributeRequest featureBrand = new ProductApiGeneralAttributeRequest("Brand", "MAC");
    
    ProductApiGeneralAttributeRequest variasi = new ProductApiGeneralAttributeRequest("Warna", "Putih");
    List<ProductApiGeneralAttributeRequest> featureList = new ArrayList<ProductApiGeneralAttributeRequest>();
    List<ProductApiGeneralAttributeRequest> variasiList = new ArrayList<ProductApiGeneralAttributeRequest>();
    variasiList.add(variasi);
    
    ProductApiGeneralAttributeRequest featureDescriptive1 = new ProductApiGeneralAttributeRequest("Resolusi Layar", "1");
    ProductApiGeneralAttributeRequest featurePredefinedOther = new ProductApiGeneralAttributeRequest("Garansi", "MAC");
    featureList.add(featureBrand);
    featureList.add(featureDescriptive1);
    featureList.add(featurePredefinedOther);
    
    productApiRequest.setVariasi(variasiList);
    productApiRequest.setFeatures(featureList);
    ImageApiRequest imageApiRequest = new ImageApiRequest();
    imageApiRequest.setLocationPath("image.png");
    imageApiRequest.setSequence(0);
    List<ImageApiRequest> imageApiRequestList = new ArrayList<ImageApiRequest>();
    imageApiRequestList.add(imageApiRequest);
    productApiRequest.setImages(imageApiRequestList);
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestAllowedValueNotFoundBuilder() {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    productApiRequest.setProductName("product-name");
    productApiRequest.setLength(1D);
    productApiRequest.setCategoryCode(CATEGORY_CODE);
    productApiRequest.setWidth(1D);
    productApiRequest.setHeight(1D);
    productApiRequest.setWeight(1D);
    productApiRequest.setDesc("desc");
    productApiRequest.setUniqueSellingPoint("unique");
    productApiRequest.setProductStory("product-story");
    productApiRequest.setUrl("url");
    productApiRequest.setUpcCode("upc-code");
    ProductApiGeneralAttributeRequest featureBrand = new ProductApiGeneralAttributeRequest("Brand", "MAC");
    ProductApiGeneralAttributeRequest featureDescriptive1 = new ProductApiGeneralAttributeRequest("Resolusi Layar", "1");
    ProductApiGeneralAttributeRequest variasi = new ProductApiGeneralAttributeRequest("Warna", "Red");
    List<ProductApiGeneralAttributeRequest> featureList = new ArrayList<ProductApiGeneralAttributeRequest>();
    List<ProductApiGeneralAttributeRequest> variasiList = new ArrayList<ProductApiGeneralAttributeRequest>();
    featureList.add(featureBrand);featureList.add(featureDescriptive1);
    variasiList.add(variasi);
    productApiRequest.setVariasi(variasiList);
    productApiRequest.setFeatures(featureList);
    ImageApiRequest imageApiRequest = new ImageApiRequest();
    imageApiRequest.setLocationPath("image.png");
    imageApiRequest.setSequence(0);
    List<ImageApiRequest> imageApiRequestList = new ArrayList<ImageApiRequest>();
    imageApiRequestList.add(imageApiRequest);
    productApiRequest.setImages(imageApiRequestList);
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestFeatureNotExistsBuilder() {
    ProductApiRequest productApiRequest = productApiRequestBuilder();
    productApiRequest.getFeatures().get(0).setName("x");
    productApiRequest.setMerchantSku("sku");
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestVarianNotExistsBuilder() {
    ProductApiRequest productApiRequest = productApiRequestBuilder();
    productApiRequest.getVariasi().get(0).setName("x");
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestFeatureNotCompleteBuilder() {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    productApiRequest.setProductName("product-name");
    productApiRequest.setLength(1D);
    productApiRequest.setCategoryCode(CATEGORY_CODE);
    productApiRequest.setWidth(1D);
    productApiRequest.setHeight(1D);
    productApiRequest.setWeight(1D);
    productApiRequest.setDesc("desc");
    productApiRequest.setUniqueSellingPoint("unique");
    productApiRequest.setProductStory("product-story");
    productApiRequest.setUrl("url");
    productApiRequest.setUpcCode("upc-code");
    ProductApiGeneralAttributeRequest featureBrand = new ProductApiGeneralAttributeRequest("Brand", "MAC");
    ProductApiGeneralAttributeRequest featureDescriptive1 = new ProductApiGeneralAttributeRequest("Resolusi Layar", "1");
    ProductApiGeneralAttributeRequest featureDescriptive2 = new ProductApiGeneralAttributeRequest("Resolusi Layar2", "2");
    ProductApiGeneralAttributeRequest variasi = new ProductApiGeneralAttributeRequest("Warna", "Putih");
    List<ProductApiGeneralAttributeRequest> featureList = new ArrayList<ProductApiGeneralAttributeRequest>();
    List<ProductApiGeneralAttributeRequest> variasiList = new ArrayList<ProductApiGeneralAttributeRequest>();
    featureList.add(featureBrand);
    featureList.add(featureDescriptive1);
    featureList.add(featureDescriptive2);
    variasiList.add(variasi);
    productApiRequest.setVariasi(variasiList);
    productApiRequest.setFeatures(featureList);
    ImageApiRequest imageApiRequest = new ImageApiRequest();
    imageApiRequest.setLocationPath("image.png");
    imageApiRequest.setSequence(0);
    List<ImageApiRequest> imageApiRequestList = new ArrayList<ImageApiRequest>();
    imageApiRequestList.add(imageApiRequest);
    productApiRequest.setImages(imageApiRequestList);
    return productApiRequest;
  }
  
  private ProductApiRequest productApiRequestVarianNotCompleteBuilder() {
    ProductApiRequest productApiRequest = new ProductApiRequest();
    productApiRequest.setProductName("product-name");
    productApiRequest.setLength(1D);
    productApiRequest.setCategoryCode(CATEGORY_CODE);
    productApiRequest.setWidth(1D);
    productApiRequest.setHeight(1D);
    productApiRequest.setWeight(1D);
    productApiRequest.setDesc("desc");
    productApiRequest.setUniqueSellingPoint("unique");
    productApiRequest.setProductStory("product-story");
    productApiRequest.setUrl("url");
    productApiRequest.setUpcCode("upc-code");
    ProductApiGeneralAttributeRequest featureBrand = new ProductApiGeneralAttributeRequest("Brand", "MAC");
    ProductApiGeneralAttributeRequest featureGaransi = new ProductApiGeneralAttributeRequest("Garansi", "MAC");
    ProductApiGeneralAttributeRequest featureDescriptive1 = new ProductApiGeneralAttributeRequest("Resolusi Layar", "1");
    ProductApiGeneralAttributeRequest variasi = new ProductApiGeneralAttributeRequest("Warna", "Putih");
    ProductApiGeneralAttributeRequest variasi2 = new ProductApiGeneralAttributeRequest("Warna2", "Hitam");
    List<ProductApiGeneralAttributeRequest> featureList = new ArrayList<ProductApiGeneralAttributeRequest>();
    List<ProductApiGeneralAttributeRequest> variasiList = new ArrayList<ProductApiGeneralAttributeRequest>();
    featureList.add(featureBrand);
    featureList.add(featureDescriptive1);
    featureList.add(featureGaransi);
    variasiList.add(variasi);
    variasiList.add(variasi2);
    productApiRequest.setVariasi(variasiList);
    productApiRequest.setFeatures(featureList);
    ImageApiRequest imageApiRequest = new ImageApiRequest();
    imageApiRequest.setLocationPath("image.png");
    imageApiRequest.setSequence(0);
    List<ImageApiRequest> imageApiRequestList = new ArrayList<ImageApiRequest>();
    imageApiRequestList.add(imageApiRequest);
    productApiRequest.setImages(imageApiRequestList);
    return productApiRequest;
  }
  
}