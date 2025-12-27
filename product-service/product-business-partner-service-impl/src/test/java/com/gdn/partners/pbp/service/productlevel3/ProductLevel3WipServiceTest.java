package com.gdn.partners.pbp.service.productlevel3;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.gdn.mta.product.entity.StateCountDTO;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductImagePredictionService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.calendar.CalendarService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3AttributeWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3AttributeWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3ItemWip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.bpconfig.ProductBusinessPartnerConfigService;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.util.ProductLevel3WipUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class ProductLevel3WipServiceTest {

  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_NAME = "productName";
  private static final String DEFAULT_BP_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_LEVEL1_ITEM_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_LEVEL1_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_EMAIL = "email@mail.com";
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final VerificationMode NEVER_CALLED = times(0);
  private static final String DEFAULT_PRODUCT_SKU = "MTA-0000001";
  private static final String DEFAULT_CATEGORY_CODE = "12345";
  private static final String DEFAULT_STORE_ID = "12";
  private static final String NOTES = "NOTES";
  private static final String PROCESS_CODE = "processCode";
  private static final String STORE_ID = "10001";
  private static final String IMAGE_URL = "url_location";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BUSINESS_PARTNER_CODE = "categoryCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String MAIL_FOR_EXCEED_ACTIVATION_FOR_PRODUCT_WIP_ID =
      "MAIL_FOR_EXCEED_ACTIVATION_FOR_PRODUCT_WIP_ID";
  private static final String MAIL_DESCRIPTION = "Permohonan Aktivasi 1 Produk";
  public static final String MAIL_SENDER = "no-reply@blibli.com";
  public static final String MESSAGE_IDENTIFIER_KEY = "username";
  public static final String IMAGE_VIOLATION = "Illegal drugs";
  public static final String TEXT_VIOLATION = "Illegal drugs Text";
  public static final String PREDICTION_TYPE = "medicine_predictions";
  public static final int PAGE =  0;
  public static final int SIZE = 10;
  public static final Pageable PAGEABLE = PageRequest.of(PAGE, SIZE);

  private ProfileResponse profileResponse;
  private ProductImageQcProcessingResponse productImageQcProcessingResponse;
  private ProductImagePrediction productImagePrediction;

  @Mock
  private ProductLevel3WipRepository productLevel3WipRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductHistoryRepository productHistoryRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private CalendarService calendarService;

  @Mock
  private ProductLevel3WipUtil productLevel3WipUtil;

  @Mock
  private ProductNotificationService productNotificationService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductBusinessPartnerConfigService productBusinessPartnerConfigService;

  @Mock
  private ProductImagePredictionService productImagePredictionService;

  @InjectMocks
  private ProductLevel3WipServiceBean productLevel3WipServiceBean;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductImageQcProcessingResponseService processingResponseService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private EmailNotificationService emailNotificationService;

  @Captor
  private ArgumentCaptor<Map> mapArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductBusinessPartnerConfigRequest> productBusinessPartnerConfigRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductLevel3WipDTO>> productLevel3WipDTOListCaptor;
  private List<ProductImageResponse> productImageResponse = new ArrayList<>();

  private ProductLevel3Wip generateProductLevel3Wip() throws Exception {
    ProductLevel3ItemWip productLevel3ItemWip = new ProductLevel3ItemWip();
    productLevel3ItemWip.setProductLevel1ItemId(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ITEM_ID);
    ProductLevel3AttributeWip productLevel3AttributeWip = new ProductLevel3AttributeWip();
    ProductLevel3Wip productLevel3Wip = new ProductLevel3Wip();
    productLevel3Wip.setCreatedDate(Calendar.getInstance().getTime());
    productLevel3Wip.setProductName(DEFAULT_PRODUCT_NAME);
    productLevel3Wip.setProductSku(DEFAULT_PRODUCT_SKU);
    productLevel3Wip.setProductLevel1Id(DEFAULT_PRODUCT_LEVEL1_ID);
    productLevel3Wip.getItems().add(productLevel3ItemWip);
    productLevel3Wip.getAttributes().add(productLevel3AttributeWip);
    return productLevel3Wip;
  }

  private ProductLevel3WipSummaryRequest generateProductLevel3WipSummaryRequest() {
    ProductLevel3WipSummaryRequest productLevel3WipSummaryRequest = new ProductLevel3WipSummaryRequest();
    productLevel3WipSummaryRequest.setCriteria(ProductLevel3WipSummaryCriteria.ALL);
    productLevel3WipSummaryRequest.setProductName(DEFAULT_PRODUCT_NAME);
    productLevel3WipSummaryRequest.setCategoryCodes(Arrays.asList(DEFAULT_CATEGORY_CODE));
    productLevel3WipSummaryRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    return  productLevel3WipSummaryRequest;
  }

  private ProductLevel3Wip generateProductLevel3WipWithState() throws Exception {
    ProductLevel3ItemWip productLevel3ItemWip = new ProductLevel3ItemWip();
    productLevel3ItemWip.setProductLevel1ItemId(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ITEM_ID);
    ProductLevel3AttributeWip productLevel3AttributeWip = new ProductLevel3AttributeWip();
    ProductLevel3Wip productLevel3Wip = new ProductLevel3Wip();
    productLevel3Wip.setProductLevel1Id(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID);
    productLevel3Wip.setCreatedDate(Calendar.getInstance().getTime());
    productLevel3Wip.getItems().add(productLevel3ItemWip);
    productLevel3Wip.getAttributes().add(productLevel3AttributeWip);
    productLevel3Wip.setState(ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name());
    return productLevel3Wip;
  }

  private Page<ProductLevel3Wip> generateProductLevel3WipWithStatePage() throws Exception {
    List<ProductLevel3Wip> productLevel3Wips = new ArrayList<ProductLevel3Wip>();
    productLevel3Wips.add(this.generateProductLevel3WipWithState());
    return new PageImpl<ProductLevel3Wip>(productLevel3Wips);
  }

  private Page<ProductLevel3Wip> generateProductLevel3WipPage() throws Exception {
    List<ProductLevel3Wip> productLevel3Wips = new ArrayList<ProductLevel3Wip>();
    productLevel3Wips.add(this.generateProductLevel3Wip());
    return new PageImpl<ProductLevel3Wip>(productLevel3Wips);
  }

  private List<ProductLevel3Wip> generateProductLevel3Wips() throws Exception {
    List<ProductLevel3Wip> productLevel3Wips = new ArrayList<ProductLevel3Wip>();
    productLevel3Wips.add(this.generateProductLevel3Wip());
    productLevel3Wips.add(this.generateProductLevel3Wip());
    return productLevel3Wips;
  }

  private List<Object[]> generateCountProductLevel3WipRaws() throws Exception {
    List<Object[]> countProductLevel3WipRaws = new ArrayList<Object[]>();
    countProductLevel3WipRaws.add(new Object[] {true, 1L});
    countProductLevel3WipRaws.add(new Object[] {false, 1L});
    return countProductLevel3WipRaws;
  }

  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(DEFAULT_PRODUCT_LEVEL1_ID);
    CategoryResponse categoryResponse = new CategoryResponse();
    ProductCategoryResponse productCategoryResponse1 = new ProductCategoryResponse();
    productCategoryResponse1.setCategory(categoryResponse);
    ProductCategoryResponse productCategoryResponse2 = new ProductCategoryResponse();
    productCategoryResponse2.setCategory(categoryResponse);
    productCategoryResponse2.setMarkForDelete(true);
    List<ProductCategoryResponse> productCategoryResponses = new ArrayList<ProductCategoryResponse>();
    productCategoryResponses.add(productCategoryResponse1);
    productCategoryResponses.add(productCategoryResponse2);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ITEM_ID);
    productDetailResponse.setProductCode(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_CODE);
    productDetailResponse.setCreatedBy(ProductLevel3WipServiceTest.DEFAULT_EMAIL);
    productDetailResponse.setProductCategoryResponses(productCategoryResponses);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    return productDetailResponse;
  }

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setProductId(DEFAULT_PRODUCT_LEVEL1_ID);
    return productCollection;
  }

  private Map<String, Object> generateNotificationDatas() throws Exception {
    Map<String, Object> notificationDatas = new HashMap<String, Object>();
    notificationDatas.put(PROCESS_CODE, "DELETE");
    return notificationDatas;
  }

  private Map<String, Object> generateNotificationReturnForCorrectionDatas() throws Exception {
    Map<String, Object> notificationDatas = new HashMap<String, Object>();
    notificationDatas.put(PROCESS_CODE, "RETURN_FOR_CORRECTION");
    notificationDatas.put("productCode" , DEFAULT_PRODUCT_CODE);
    notificationDatas.put("notes" , NOTES);
    return notificationDatas;
  }

  private ProfileResponse generateProfileResponse() throws Exception {
    ResponsiblePersonDTO responsiblePersonDTO = new ResponsiblePersonDTO();
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setResponsiblePerson(responsiblePersonDTO);
    return profileResponse;
  }

  private UpdateProductLevel3Wip generateUpdateProductLevel3Wip() {
    UpdateProductLevel3Wip product = new UpdateProductLevel3Wip();
    product.setItems(new ArrayList<>());
    for (int i = 0; i < 5; i++) {
      UpdateProductLevel3ItemWip item = new UpdateProductLevel3ItemWip();
      item.setGdnSku("item-" + i);
      item.setStock(10);
      product.getItems().add(item);
    }
    product.setAttributes(new ArrayList<>());
    for (int i = 0; i < 2; i++) {
      UpdateProductLevel3AttributeWip attribute = new UpdateProductLevel3AttributeWip();
      attribute.setAttributeId("attribute-" + i);
      attribute.setValue("attribute " + i);
      product.getAttributes().add(attribute);
    }
    return product;
  }

  private ProductLevel3Wip generateProductLevel3WipForResubmit() throws Exception {
    ProductLevel3Wip product = new ProductLevel3Wip();
    product.setItems(new ArrayList<>());
    for (int i = 0; i < 5; i++) {
      ProductLevel3ItemWip item = new ProductLevel3ItemWip();
      item.setGdnSku("item-" + i);
      item.setStock(10);
      product.getItems().add(item);
    }
    product.setAttributes(new ArrayList<>());
    for (int i = 0; i < 2; i++) {
      ProductLevel3AttributeWip attribute = new ProductLevel3AttributeWip();
      attribute.setAttributeId("attribute-" + i);
      attribute.setValue("update attribute " + i);
      product.getAttributes().add(attribute);
    }
    return product;
  }

  private ProfileResponse generateBusinessPartner() throws Exception {
    ProfileResponse businessPartner = new ProfileResponse();
    businessPartner.setCompany(new CompanyDTO());
    businessPartner.getCompany().setInternationalFlag(true);
    return businessPartner;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BP_CODE);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    profileResponse.setCompany(companyDTO);

    MockitoAnnotations.initMocks(this);
    Page<ProductLevel3Wip> productLevel3WipPage = this.generateProductLevel3WipPage();
    List<ProductLevel3Wip> productLevel3Wips = this.generateProductLevel3Wips();
    List<Object[]> countProductLevel3WipRaws = this.generateCountProductLevel3WipRaws();
    ProductDetailResponse productDetailResponse = this.generateProductDetailResponse();
    ProductCollection productCollection = this.generateProductCollection();
    ProductLevel3WipSummaryRequest productLevel3WipSummaryRequest = generateProductLevel3WipSummaryRequest();
    productLevel3WipSummaryRequest.setCriteria(ProductLevel3WipSummaryCriteria.FAILED);
    when(
        this.productLevel3WipRepository
            .findSummaryByFilterWithState(any(), eq(productLevel3WipSummaryRequest), any()))
        .thenReturn(productLevel3WipPage);
    when(this.productRepository.findProductDetailByProductCode(any())).thenReturn(productDetailResponse);
    when(
        this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(any(),
            any())).thenReturn(productLevel3Wips);
    when(this.productLevel3WipRepository.saveAll(anyList())).thenReturn(null);
    when(
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(),
            any())).thenReturn(productCollection);
    when(this.productLevel3WipRepository.findByStoreIdAndProductLevel1Id(any() , any()))
        .thenReturn(productLevel3Wips);

    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductImageResponse image1 = ProductImageResponse.builder().locationPath(IMAGE_URL).mainImage(true).originalImage(false)
      .productId(DEFAULT_PRODUCT_LEVEL1_ID).build();
    ProductImageResponse image2 =
      ProductImageResponse.builder().locationPath("url2").mainImage(true).productId(DEFAULT_PRODUCT_LEVEL1_ID)
        .originalImage(false).build();
    ProductImageResponse image5 =
        ProductImageResponse.builder().locationPath("url5").mainImage(false).productId(DEFAULT_PRODUCT_LEVEL1_ID)
            .build();
    ProductImageResponse image6 =
        ProductImageResponse.builder().locationPath("url6").mainImage(false).productId(DEFAULT_PRODUCT_LEVEL1_ID)
            .build();
    ProductImageResponse image3 =
        ProductImageResponse.builder().locationPath("url3").mainImage(false).productId(DEFAULT_PRODUCT_LEVEL1_ID)
            .build();
    ProductImageResponse image4 =
        ProductImageResponse.builder().locationPath("url3").mainImage(false).productId(DEFAULT_PRODUCT_CODE).build();
    productImagePrediction =
        ProductImagePrediction.builder().predictionType(PREDICTION_TYPE).displayName(IMAGE_VIOLATION).build();
    productImageQcProcessingResponse =
        ProductImageQcProcessingResponse.builder().productCode(DEFAULT_PRODUCT_CODE).forceReview(true)
            .imageViolations(IMAGE_VIOLATION).build();
    productImageResponse.addAll(Arrays.asList(image1, image2, image3, image4, image5, image6));
    when(productOutbound
        .filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE))
        .thenReturn(productImageResponse);
    when(productCollectionRepository.findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID))).thenReturn(Arrays.asList(productCollection));
    when(processingResponseService.findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_CODE))).thenReturn(
      Arrays.asList(productImageQcProcessingResponse));
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
      STORE_ID);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    verifyNoMoreInteractions(this.productLevel3WipRepository);
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.businessPartnerRepository);
    verifyNoMoreInteractions(this.productCollectionRepository);
    verifyNoMoreInteractions(this.calendarService);
    verifyNoMoreInteractions(this.productNotificationService);
    verifyNoMoreInteractions(this.productBusinessPartnerConfigService);
    verifyNoMoreInteractions(this.mandatoryParameterHelper);
    verifyNoMoreInteractions(this.productOutbound);
    verifyNoMoreInteractions(this.processingResponseService);
    verifyNoMoreInteractions(this.productImagePredictionService);
    verifyNoMoreInteractions(emailNotificationService);
    verifyNoMoreInteractions(this.productBusinessPartnerService);
  }

  @Test
  public void findSummaryByFilterStateTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.ALL);
    this.productLevel3WipServiceBean
        .findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository)
        .findSummaryByFilterWithState(any(), Mockito.eq(request), any());
  }

  @Test
  @Disabled
  public void findSummaryByFilterStateMainImageResponseNullTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("ANY NOTES");
    ProductImageQcProcessingResponse productImageQcProcessingResponse =
        ProductImageQcProcessingResponse.builder().productCode(DEFAULT_PRODUCT_CODE).forceReview(true)
            .imageViolations(IMAGE_VIOLATION).textViolations(TEXT_VIOLATION).build();
    productImageResponse.forEach(p -> p.setOriginalImage(true));
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any()))
        .thenReturn(this.generateProductLevel3WipWithStatePage());
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(productHistory);
    when(productOutbound.filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE))
        .thenReturn(productImageResponse);
    when(productImagePredictionService.findByStoreIdAndForceReviewTrue(Mockito.any()))
        .thenReturn(new ArrayList<>());
    when(processingResponseService.findByStoreIdAndProductCodeIn(null, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productImageQcProcessingResponse));
    Page<ProductLevel3WipDTO> response = this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any(Pageable.class));
    verify(productOutbound).filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Mockito.any(), Mockito.any());
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Mockito.any(), Mockito.any());
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    Assertions.assertTrue(response.getContent().get(0).getForceReviewImageViolations().stream().anyMatch(
        productImageQcProcessingResponse1 -> Constants.SUSPICIOUS_BRAND
            .equals(productImageQcProcessingResponse1.getEnName())));
    Assertions.assertEquals(2, response.getContent().get(0).getForceReviewImageViolations().size(), 0);
   }

  @Test
  @Disabled
  public void findSummaryByFilterStateForceReviewNullTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("ANY NOTES");
    ProductImageQcProcessingResponse productImageQcProcessingResponse =
        ProductImageQcProcessingResponse.builder().productCode(DEFAULT_PRODUCT_CODE).forceReview(true)
            .imageViolations(IMAGE_VIOLATION).textViolations(TEXT_VIOLATION).build();
    productImageResponse.forEach(p -> p.setOriginalImage(true));
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any()))
        .thenReturn(this.generateProductLevel3WipWithStatePage());
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(anyString(),
            anyString(), anyString())).thenReturn(productHistory);
    when(productOutbound.filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE))
        .thenReturn(productImageResponse);
    when(productImagePredictionService.findByStoreIdAndForceReviewTrue(Mockito.any()))
        .thenReturn(new ArrayList<>());
    when(processingResponseService.findByStoreIdAndProductCodeIn(null, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productImageQcProcessingResponse));
    Page<ProductLevel3WipDTO> response = this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any());
    verify(productOutbound).filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Mockito.any(), Mockito.any());
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Mockito.any(), Mockito.any());
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    verify(productImagePredictionService).findByStoreIdAndForceReviewTrue(Mockito.any());
  }

  @Test
  public void findSummaryByFilterStateForceReviewNullImageViolationNullTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("ANY NOTES");
    ProductImageQcProcessingResponse productImageQcProcessingResponse =
        ProductImageQcProcessingResponse.builder().productCode(DEFAULT_PRODUCT_CODE).forceReview(true)
            .textViolations(TEXT_VIOLATION).build();
    productImageResponse.forEach(p -> p.setOriginalImage(true));
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any(Pageable.class)))
        .thenReturn(this.generateProductLevel3WipWithStatePage());
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(productHistory);
    when(productOutbound.filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE))
        .thenReturn(productImageResponse);
    when(productImagePredictionService.findByStoreIdAndForceReviewTrue(Mockito.any()))
        .thenReturn(new ArrayList<>());
    when(processingResponseService.findByStoreIdAndProductCodeIn(null, Arrays.asList(DEFAULT_PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productImageQcProcessingResponse));
    Page<ProductLevel3WipDTO> response = this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PageRequest.of(0, 1));
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any());
    verify(productOutbound).filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Mockito.any(), Mockito.any());
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Mockito.any(), Mockito.any());
    verify(productImagePredictionService).findByStoreIdAndForceReviewTrue(Mockito.any());
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
  }

  @Test
  public void findSummaryByFilterStateInProgressTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.IN_PROGRESS);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PageRequest.of(0,1));
    verify(this.productLevel3WipRepository)
        .findSummaryByFilterWithState(any(), eq(request), any(Pageable.class));
  }

  @Test
  public void findSummaryByFilterStateNeedCorrectionTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("ANY NOTES");
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any(Pageable.class)))
        .thenReturn(this.generateProductLevel3WipWithStatePage());
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(productHistory);
    Page<ProductLevel3WipDTO> response = this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PageRequest.of(0, 1));
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any(Pageable.class));
    verify(productOutbound)
        .filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID));
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    verify(productImagePredictionService).findByStoreIdAndForceReviewTrue(Mockito.any());
    Assertions.assertEquals(IMAGE_URL, response.getContent().get(0).getProductMainImage());
  }

  @Test
  public void findSummaryByFilterStateConditionsTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    Page<ProductLevel3Wip> productLevel3Wips = this.generateProductLevel3WipWithStatePage();
    productLevel3Wips.getContent().get(0).setState(ProductLevel3WipSummaryCriteria.IN_PROGRESS.name());
    ProductHistory productHistory = new ProductHistory();
    productHistory.setNotes("ANY NOTES");
    when(productOutbound
        .filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE))
        .thenReturn(new ArrayList<>());
    when(productCollectionRepository.findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID)))
        .thenReturn(new ArrayList<>());
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any()))
        .thenReturn(productLevel3Wips);
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(productHistory);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any());
    verify(productOutbound)
        .filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID));
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList());
  }

  @Test
  public void findSummaryByFilterStateNeedCorrectionEmptyListTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    Page<ProductLevel3Wip> response = new PageImpl<>(new ArrayList<>());
    when(this.productLevel3WipRepository
        .findSummaryByFilterWithState(any(),Mockito.eq(request), any())).thenReturn(response);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), Mockito.eq(request), any());
  }

  @Test
  public void findSummaryByFilterStateNeedCorrectionNullHistoryTest() throws Exception {
    productImageQcProcessingResponse.setImageViolations(Constants.CATEGORY_MISMATCH);
    when(processingResponseService.findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_CODE))).thenReturn(
      Arrays.asList(productImageQcProcessingResponse));
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    ProductHistory productHistory = null;
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request), any()))
        .thenReturn(this.generateProductLevel3WipWithStatePage());
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(productHistory);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PageRequest.of(0,1));
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any());
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    verify(productOutbound)
        .filterProductImagesByProductIds(Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID));
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID, Arrays.asList(DEFAULT_PRODUCT_CODE));
    verify(productImagePredictionService).findByStoreIdAndForceReviewTrue(any());
  }

  @Test
  public void findSummaryByFilterWithStateImageViolationEmptyTest() throws Exception {
    productImageQcProcessingResponse.setImageViolations(StringUtils.EMPTY);
    when(processingResponseService.findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_CODE))).thenReturn(
      Arrays.asList(productImageQcProcessingResponse));
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    ProductHistory productHistory = null;
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), eq(request),
      any())).thenReturn(this.generateProductLevel3WipWithStatePage());
    when(
      this.productHistoryRepository.findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
        any(), any(), any())).thenReturn(productHistory);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PageRequest.of(0, 1));
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), eq(request), any());
    verify(
      this.productHistoryRepository).findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
      any(), eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    verify(productOutbound).filterProductImagesByProductIds(
      Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID), Boolean.TRUE);
    verify(productCollectionRepository).findByStoreIdAndProductIds(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_LEVEL1_ID));
    verify(processingResponseService).findByStoreIdAndProductCodeIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(DEFAULT_PRODUCT_CODE));
    verify(productImagePredictionService).findByStoreIdAndForceReviewTrue(any());
  }

  @Test
  public void findSummaryByFilterStateNeedCorrectionNullResponseTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.NEED_CORRECTION);
    ProductHistory productHistory = null;
    when(this.productLevel3WipRepository.findSummaryByFilterWithState(any(), Mockito.eq(request), any()))
        .thenReturn(null);
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(), any(), any()))
        .thenReturn(productHistory);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository).findSummaryByFilterWithState(any(), Mockito.eq(request), any());
  }

  @Test
  public void findSummaryByFilterStateFailedTest() throws Exception {
    ProductLevel3WipSummaryRequest request = generateProductLevel3WipSummaryRequest();
    request.setCriteria(ProductLevel3WipSummaryCriteria.FAILED);
    this.productLevel3WipServiceBean.findSummaryByFilterWithState(request, PAGEABLE);
    verify(this.productLevel3WipRepository)
        .findSummaryByFilterWithState(any(), eq(request), any());
  }

  @Test
  public void countSummaryWithStateTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3WipServiceBean,
      "newFlowForFetchingLevel3CountEnabled", true);
    this.productLevel3WipServiceBean.countSummaryWithState(DEFAULT_BP_CODE);
    verify(this.productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActiveAndStateAndMarkForDeleteFalse(
        any(), eq(DEFAULT_BP_CODE));
    verify(this.productLevel3WipUtil).generateCountProductLevel3WipWithState(anyList());
  }

  @Test
  public void countSummaryWithStateAndSwitchOnTest() throws Exception {
    this.productLevel3WipServiceBean.countSummaryWithState(DEFAULT_BP_CODE);
    verify(this.productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActiveAndStateAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    verify(this.productLevel3WipUtil).generateCountProductLevel3WipWithState(anyList());
  }

  @Test
  public void countSummaryByFilterTypeTest() throws Exception {
    when(productLevel3WipRepository.countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(STORE_ID,
        DEFAULT_BP_CODE)).thenReturn(new ArrayList<>());
    this.productLevel3WipServiceBean.countSummaryByFilterType(DEFAULT_BP_CODE, STORE_ID, Constants.PRIMARY);
    verify(this.productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(
        any(), eq(DEFAULT_BP_CODE));
    verify(this.productLevel3WipUtil).generateCountProductLevel3ByState(anyList());
  }

  @Test
  public void countSummaryByFilterTypeTestWithSwitchOn() throws Exception {
    ReflectionTestUtils.setField(productLevel3WipServiceBean,
      "newFlowForFetchingLevel3CountEnabled", true);
    when(
      productLevel3WipRepository.countByStoreIdAndBusinessPartnerCodeAndActivatedFalseAndMarkForDeleteFalse(
        STORE_ID, DEFAULT_BP_CODE)).thenReturn(
      List.of(StateCountDTO.builder().count(888L).state(Constants.IN_PROGRESS_STATE).build()));
    this.productLevel3WipServiceBean.countSummaryByFilterType(DEFAULT_BP_CODE, STORE_ID,
      Constants.PRIMARY);
    verify(
      this.productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActivatedFalseAndMarkForDeleteFalse(
      STORE_ID, DEFAULT_BP_CODE);
    verify(this.productLevel3WipUtil).generateProductLevel3CountByState(
      List.of(StateCountDTO.builder().count(888L).state(Constants.IN_PROGRESS_STATE).build()));
  }

  @Test
  public void countSummaryByFilterTypeSecondaryTest() throws Exception {
    when(productLevel3WipRepository.countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(STORE_ID,
        DEFAULT_BP_CODE)).thenReturn(new ArrayList<>());
    this.productLevel3WipServiceBean.countSummaryByFilterType(DEFAULT_BP_CODE, STORE_ID, Constants.SECONDARY);
    verify(this.productBusinessPartnerService).countRejectedProductsByBusinessPartnerId(
        any(), eq(DEFAULT_BP_CODE));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void updateTest() throws Exception {
    this.productLevel3WipServiceBean.update(null);
    verify(this.productRepository).findProductDetailByProductCode(any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), any());
    verify(this.productLevel3WipRepository).saveAll(any());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void deleteTest() throws Exception {
    this.productLevel3WipServiceBean.delete(null);
    verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), any());
    verify(this.productLevel3WipRepository).saveAll(any());
  }

  @Test
  public void deleteTest_alreadyDeleted() throws Exception {
    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), Mockito.eq(DEFAULT_PRODUCT_CODE))).thenReturn(null);
    this.productLevel3WipServiceBean.delete(DEFAULT_PRODUCT_CODE);
    verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), Mockito.eq(DEFAULT_PRODUCT_CODE));
  }

  @SuppressWarnings("unchecked")
  @Test
  public void deleteWithEmptyProductLevel3WipsTest() throws Exception {
    when(this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(any(),
            any())).thenReturn(new ArrayList<ProductLevel3Wip>());
    this.productLevel3WipServiceBean.delete(null);
    verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        any(), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), any());
    verify(this.productLevel3WipRepository, ProductLevel3WipServiceTest.NEVER_CALLED).saveAll(any());
  }

  @Test
  public void findByProductSkuWithIsActiveFalseTest() throws Exception {
    MDC.put("storeId", "10001");
    ProductLevel3Wip productLevel3Wip = generateProductLevel3Wip();
    when(this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_SKU))
        .thenReturn(productLevel3Wip);
    this.productLevel3WipServiceBean.findByProductSku(DEFAULT_PRODUCT_SKU, false);
    verify(this.productLevel3WipRepository, times(1))
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_PRODUCT_SKU);
    MDC.clear();
  }

  @Test
  public void findByProductSkuWithIsActiveTrueTest() throws Exception {
    MDC.put("storeId", "10001");
    ProductLevel3Wip productLevel3Wip = generateProductLevel3Wip();
    when(this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteTrue(STORE_ID, DEFAULT_PRODUCT_SKU))
        .thenReturn(productLevel3Wip);
    this.productLevel3WipServiceBean.findByProductSku(DEFAULT_PRODUCT_SKU, true);
    verify(this.productLevel3WipRepository, times(1))
        .findByStoreIdAndProductSkuAndMarkForDeleteTrue(STORE_ID, DEFAULT_PRODUCT_SKU);
    MDC.clear();
  }

  @Test
  public void returnDraftForCorrection() throws Exception {
    ProductCollection productCollection = this.generateProductCollection();
    productCollection.setProductId(DEFAULT_PRODUCT_LEVEL1_ITEM_ID);
    List<ProductLevel3Wip> productLevel3Wips = this.generateProductLevel3Wips();
    when(this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    when(this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), eq(DEFAULT_PRODUCT_LEVEL1_ITEM_ID))).thenReturn(productLevel3Wips);
    this.productLevel3WipServiceBean.returnDraftForCorrection(DEFAULT_PRODUCT_CODE , NOTES);
    verify(this.productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE));
    verify(this.productLevel3WipRepository).findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), eq(DEFAULT_PRODUCT_LEVEL1_ITEM_ID));
    verify(this.productLevel3WipRepository, atLeastOnce()).saveAll(productLevel3Wips);
    verify(this.productNotificationService, times(2))
        .sendProductReturnForCorrectionNotification(any(), eq(productCollection.getProductName()),
            eq(DEFAULT_PRODUCT_SKU), eq(NOTES));
  }

  @Test
  public void returnDraftForCorrection_EmptyResult() throws Exception {
    ProductCollection productCollection = this.generateProductCollection();
    productCollection.setProductId(DEFAULT_PRODUCT_LEVEL1_ITEM_ID);
    when(this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), eq(DEFAULT_PRODUCT_CODE)))
            .thenReturn(productCollection);
    when(this.productLevel3WipRepository.findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), eq(DEFAULT_PRODUCT_LEVEL1_ITEM_ID))).thenReturn(new ArrayList<>());
    this.productLevel3WipServiceBean.returnDraftForCorrection(DEFAULT_PRODUCT_CODE , NOTES);
    verify(this.productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(any(), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(
        any(), any());
  }

  @Test
  public void resubmitTest() throws Exception {
    ProductRequest productRequest = generateProductRequest();
    Mockito.when(this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(this.generateProductLevel3WipForResubmit());
    when(this.calendarService
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any()))
        .thenReturn(new Date());
    this.productLevel3WipServiceBean
        .resubmit(productRequest, this.generateUpdateProductLevel3Wip(), new Date());
    Mockito.verify(this.productLevel3WipRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3WipRepository).save(Mockito.any());
    Mockito.verify(this.calendarService)
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any());
  }

  @Test
  public void resubmitTest_withEmptyProductCategory() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    ProductLevel3Wip productLevel3Wip = this.generateProductLevel3WipForResubmit();
    Mockito.when(this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(this.generateProductLevel3WipForResubmit());
    this.productLevel3WipServiceBean
        .resubmit(productRequest, this.generateUpdateProductLevel3Wip(), new Date());
    Mockito.verify(this.productLevel3WipRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3WipRepository).save(Mockito.any(ProductLevel3Wip.class));
  }

  private ProductRequest generateProductRequest() {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setId(DEFAULT_PRODUCT_LEVEL1_ID);
    productRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    productRequest.setProductCategories(Arrays.asList(generateProductCategoryRequest()));
    return productRequest;
  }

  private ProductCategoryRequest generateProductCategoryRequest() {
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    productCategoryRequest.setId(DEFAULT_PRODUCT_LEVEL1_ID);
    productCategoryRequest.setCategory(generateCategoryRequest());
    return productCategoryRequest;
  }

  private CategoryRequest generateCategoryRequest() {
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setInternalActivationInterval(72);
    categoryRequest.setId(DEFAULT_PRODUCT_LEVEL1_ID);
    categoryRequest.setCategoryCode(DEFAULT_CATEGORY_CODE);
    return categoryRequest;
  }

  @Test
  public void resubmitTestException() throws Exception {
    Mockito.when(this.productLevel3WipRepository
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any())).thenReturn(null);
    try {
      this.productLevel3WipServiceBean
          .resubmit(new ProductRequest(), this.generateUpdateProductLevel3Wip(), new Date());
    } catch (Exception e) {
      Mockito.verify(this.productLevel3WipRepository)
          .findByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void findCountByExceedingActivationDateTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    Mockito.when(productLevel3WipRepository
        .findProductWipCountByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
            date)).thenReturn(1);
    int count = productLevel3WipServiceBean
        .findCountByExceedingActivationDate(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    Mockito.verify(productLevel3WipRepository)
        .findProductWipCountByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
            date);
    Assertions.assertEquals(count, 1);
  }

  @Test
  public void findCountByExceedingActivationDateTest_fail() throws Exception {
    Date date = Calendar.getInstance().getTime();
    Mockito.when(productLevel3WipRepository
        .findProductWipCountByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
            date)).thenThrow(new ApplicationRuntimeException());
    try {
      productLevel3WipServiceBean
          .findCountByExceedingActivationDate(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(productLevel3WipRepository)
          .findProductWipCountByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
              date);
    }
  }

  @Test
  public void findProductWipByExpectationActivationDateGreater() throws Exception {
    Date date = Calendar.getInstance().getTime();
    ProductLevel3Wip wip = generateProductLevel3Wip();
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date))
        .thenReturn(Arrays.asList(wip));
    List<ProductLevel3WipDTO> dtos = productLevel3WipServiceBean
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    Mockito.verify(productLevel3WipRepository)
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    Assertions.assertNotNull(dtos);
    Assertions.assertEquals(dtos.size(), 1);
    Assertions.assertEquals(dtos.get(0).getProductName(), DEFAULT_PRODUCT_NAME);
  }

  @Test
  public void findProductWipByExpectationActivationDateGreater_NEED_CORRECTION() throws Exception {
    Date date = Calendar.getInstance().getTime();
    ProductLevel3Wip wip = generateProductLevel3Wip();
    wip.setState(ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name());
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date))
        .thenReturn(Arrays.asList(wip));
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(new ProductHistory());
    List<ProductLevel3WipDTO> dtos = productLevel3WipServiceBean
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    Mockito.verify(productLevel3WipRepository)
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    Assertions.assertNotNull(dtos);
    Assertions.assertEquals(dtos.size(), 1);
    Assertions.assertEquals(dtos.get(0).getProductName(), DEFAULT_PRODUCT_NAME);
  }

  @Test
  public void findProductWipByExpectationActivationDateGreater_historyNull() throws Exception {
    Date date = Calendar.getInstance().getTime();
    ProductLevel3Wip wip = generateProductLevel3Wip();
    wip.setState(ProductLevel3WipSummaryCriteria.NEED_CORRECTION.name());
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date))
        .thenReturn(Arrays.asList(wip));
    when(this.productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            any(), any())).thenReturn(null);
    List<ProductLevel3WipDTO> dtos = productLevel3WipServiceBean
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    Mockito.verify(productLevel3WipRepository)
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date);
    verify(this.productHistoryRepository)
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(any(),
            eq(ProductLevel3WipServiceTest.DEFAULT_PRODUCT_LEVEL1_ID), any());
    Assertions.assertNotNull(dtos);
    Assertions.assertEquals(dtos.size(), 1);
    Assertions.assertEquals(dtos.get(0).getProductName(), DEFAULT_PRODUCT_NAME);
  }

  @Test
  public void findProductWipByExpectationActivationDateGreater_failed() throws Exception {
    Date date = Calendar.getInstance().getTime();
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE, date))
        .thenThrow(new ApplicationRuntimeException());
    try {
      productLevel3WipServiceBean
          .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
              date);
    } catch (ApplicationRuntimeException e) {
      Mockito.verify(productLevel3WipRepository)
          .findProductWipByExpectationActivationDateGreater(DEFAULT_STORE_ID, DEFAULT_BP_CODE,
              date);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeTest() throws Exception {
    when(this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
        DEFAULT_PRODUCT_CODE)).thenReturn(generateProductCollection());
    this.productLevel3WipServiceBean.findProductL3WipByStoreIdAndProductCode(DEFAULT_STORE_ID ,
        DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCode(Mockito.eq(DEFAULT_STORE_ID),
        Mockito.eq(DEFAULT_PRODUCT_CODE));
    Mockito.verify(this.productLevel3WipRepository)
        .findByStoreIdAndProductLevel1Id(eq(DEFAULT_STORE_ID) ,
            eq(DEFAULT_PRODUCT_LEVEL1_ID));
  }

  @Test
  public void sendMailForEmailExceededActivationTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BP_CODE))
        .thenReturn(profileResponse);
    ProductLevel3Wip wip = generateProductLevel3Wip();
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(Mockito.any(), Mockito.eq(DEFAULT_BP_CODE),
            Mockito.any())).thenReturn(Arrays.asList(wip));
    Mockito.doNothing().when(productBusinessPartnerConfigService)
        .save(Mockito.any(), Mockito.eq(DEFAULT_USERNAME), Mockito.any());
    productLevel3WipServiceBean.sendMailForEmailExceededActivation(DEFAULT_BP_CODE, DEFAULT_USERNAME);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BP_CODE);
    Mockito.verify(productLevel3WipRepository)
        .findProductWipByExpectationActivationDateGreater(Mockito.any(), Mockito.eq(DEFAULT_BP_CODE),
            Mockito.any());
    Mockito.verify(productBusinessPartnerConfigService).save(Mockito.any(), Mockito.eq(DEFAULT_USERNAME),
        productBusinessPartnerConfigRequestArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_BP_CODE, productBusinessPartnerConfigRequestArgumentCaptor.getValue().getBpCode());
    Assertions.assertNotNull(productBusinessPartnerConfigRequestArgumentCaptor.getValue().getProductToActivateNotifyMailDate());
    Mockito.verify(emailNotificationService).sendExceedActivationEmail(Mockito.eq(profileResponse),
        Mockito.eq(DEFAULT_USERNAME), productBusinessPartnerConfigRequestArgumentCaptor.capture(),
        productLevel3WipDTOListCaptor.capture(), Mockito.any());
  }

  @Test
  public void sendMailForEmailExceededActivationExceptionTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BP_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productLevel3WipRepository
        .findProductWipByExpectationActivationDateGreater(Mockito.any(), Mockito.eq(DEFAULT_BP_CODE),
            Mockito.any())).thenReturn(Collections.emptyList());
    Mockito.doThrow(ApplicationRuntimeException.class).when(emailNotificationService)
        .sendExceedActivationEmail(Mockito.eq(profileResponse), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(), Mockito.any(), Mockito.any());
   try {
     Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
       productLevel3WipServiceBean.sendMailForEmailExceededActivation(DEFAULT_BP_CODE, DEFAULT_USERNAME);
     });
   }
   finally {
     Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BP_CODE);
     Mockito.verify(productLevel3WipRepository)
         .findProductWipByExpectationActivationDateGreater(Mockito.any(), Mockito.eq(DEFAULT_BP_CODE), Mockito.any());
     Mockito.verify(emailNotificationService).sendExceedActivationEmail(Mockito.eq(profileResponse),
         Mockito.eq(DEFAULT_USERNAME), productBusinessPartnerConfigRequestArgumentCaptor.capture(),
         productLevel3WipDTOListCaptor.capture(), Mockito.any());
   }
  }

  @Test
  public void getProductLevel3WipByProductSkuWithItemsInitialisedTest() throws Exception {
    ProductLevel3Wip productLevel3Wip = generateProductLevel3Wip();
    when(this.productLevel3WipRepository
        .findByStoreIdAndProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU))
        .thenReturn(productLevel3Wip);
    ProductLevel3Wip response = productLevel3WipServiceBean
        .getProductLevel3WipByProductSkuWithItemsInitialised(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU);
    Mockito.verify(productLevel3WipRepository)
        .findByStoreIdAndProductSku(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, response.getProductSku());
  }

  @Test
  public void countSummaryWithStateCachedTest() throws Exception {
    productLevel3WipServiceBean.countSummaryWithStateCached(DEFAULT_BP_CODE);
    verify(
      productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActiveAndStateAndMarkForDeleteFalse(
      Constants.DEFAULT_STORE_ID, DEFAULT_BP_CODE);
    verify(this.productLevel3WipUtil).generateCountProductLevel3WipWithState(anyList());
  }

  @Test
  public void countSummaryWithFilterTypeCachedTest() throws Exception {
    productLevel3WipServiceBean.countSummaryByFilterTypeCached(DEFAULT_BP_CODE,
      Constants.DEFAULT_STORE_ID, Constants.PRIMARY);
    verify(this.productLevel3WipRepository).countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(
      any(), eq(DEFAULT_BP_CODE));
    verify(this.productLevel3WipUtil).generateCountProductLevel3ByState(anyList());
  }

}
