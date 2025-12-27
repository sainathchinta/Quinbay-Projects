package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.entity.ProductOptimisationSuggestionFeedback;
import com.gdn.partners.product.analytics.entity.ProductOptimisationSuggestions;
import com.gdn.partners.product.analytics.entity.ProductOptimiseFeedback;
import com.gdn.partners.product.analytics.entity.SystemParameter;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.dto.SuggestionImpactDto;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.ProductOptimisationRepository;
import com.gdn.partners.product.analytics.repository.ProductOptimiseFeedbackRepository;
import com.gdn.partners.product.analytics.repository.SystemParameterRepository;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationListResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.product.analytics.web.model.request.SuggestionFeedbackRequest;
import model.ProductChangeEventModel;
import model.ProductOptimisationEventModel;
import model.ProductOptimisationSuggestionsModel;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;

class ProductOptimisationServiceImplTest {

  @InjectMocks
  private ProductOptimisationServiceImpl productOptimisationService;
  @Mock
  private ProductOptimisationRepository productOptimisationRepository;
  @Mock
  private SystemParameterRepository systemParameterRepository;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private KafkaPublisher kafkaPublisher;

  @Captor
  private ArgumentCaptor<ProductOptimisationDetails> productOptimisationDetailsArgumentCaptor;
  @Mock
  private ProductOptimiseFeedbackRepository productOptimiseFeedbackRepository;
  private ProductChangeEventModel productChangeEventModel;
  private ProductOptimisationDetails productOptimisationDetails;
  private ProductOptimisationEventModel eventModel;
  private ProductOptimisationSuggestionsModel suggestionsModel;
  private ProductOptimisationSuggestions optimisationSuggestions;
  private ProductOptimisationSuggestionFeedback productOptimisationSuggestionFeedback;
  private ProductOptimiseFeedback productOptimiseFeedback;
  private ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest;
  private SuggestionFeedbackRequest suggestionFeedbackRequest;
  private SystemParameter systemParameter;
  private SuggestionImpactDto suggestionImpactDto;
  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "storeId";
  private static final String SELLER_CODE = "SELLER_CODE";
  private static final String DESCRIPTION = "description";
  private static final String IMPACT = "impact-product";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String SUGGESTION_TYPE = "suggestionType";
  private static final String IMAGE_URL = "image-url";
  private static final String EVENT = "event";

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    productChangeEventModel = new ProductChangeEventModel();
    productChangeEventModel.setProductSku(PRODUCT_SKU);
    productOptimisationDetails = new ProductOptimisationDetails();
    productOptimisationDetails.setProductSku(PRODUCT_SKU);
    productOptimisationDetails.setImageUrl(IMAGE_URL);
    productOptimisationDetails.setSellerCode(SELLER_CODE);


    eventModel = new ProductOptimisationEventModel();
    suggestionsModel = new ProductOptimisationSuggestionsModel();
    eventModel.setProductSku(PRODUCT_SKU);
    eventModel.setSuggestions(List.of(suggestionsModel));
    productOptimisationFeedbackRequest = new ProductOptimisationFeedbackRequest();
    productOptimiseFeedback = new ProductOptimiseFeedback();
    productOptimiseFeedback.setProductSku(PRODUCT_SKU);
    productOptimisationFeedbackRequest.setProductSku(PRODUCT_SKU);
    suggestionFeedbackRequest = new SuggestionFeedbackRequest();
    suggestionFeedbackRequest.setSuggestionName(SUGGESTION_TYPE);
    suggestionFeedbackRequest.setFeedbackType(Boolean.TRUE);
    productOptimisationSuggestionFeedback = new ProductOptimisationSuggestionFeedback();
    productOptimisationSuggestionFeedback.setFeedbackType(Boolean.TRUE);
    productOptimisationSuggestionFeedback.setSuggestionName(SUGGESTION_TYPE);
    productOptimiseFeedback.setSuggestionFeedback(Collections.singletonList(productOptimisationSuggestionFeedback));
    productOptimisationFeedbackRequest.setSuggestionFeedback(Collections.singletonList(suggestionFeedbackRequest));

    systemParameter = new SystemParameter();
    systemParameter.setDescription(DESCRIPTION);

    systemParameter.setVariable(SUGGESTION_TYPE);
    systemParameter.setStoreId(STORE_ID);

    suggestionImpactDto = new SuggestionImpactDto();
    systemParameter.setValue(suggestionImpactDto.toString());


    optimisationSuggestions = new ProductOptimisationSuggestions();
    optimisationSuggestions.setSuggestionType(SUGGESTION_TYPE);
    productOptimisationDetails.setSuggestions(List.of(optimisationSuggestions));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productOptimisationRepository);
    Mockito.verifyNoMoreInteractions(productOptimiseFeedbackRepository);
    Mockito.verifyNoMoreInteractions(systemParameterRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  void removeDeletedProduct_productNotPresent() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void removeDeletedProduct_emptyProductChangeType() {
    productChangeEventModel.setProductChangeEventType(Collections.EMPTY_LIST);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_suspendedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.SUSPEND_FLAG_CHANGE));
    productChangeEventModel.setSuspended(true);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_reactivatedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.SUSPEND_FLAG_CHANGE));
    productChangeEventModel.setSuspended(false);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_archivedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.ARCHIVE_FLAG_CHANGE));
    productChangeEventModel.setArchived(true);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_unArchivedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.ARCHIVE_FLAG_CHANGE));
    productChangeEventModel.setArchived(false);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_takenDownProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.FORCE_REVIEW_FLAG_CHANGE));
    productChangeEventModel.setForceReview(true);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_activatedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.FORCE_REVIEW_FLAG_CHANGE));
    productChangeEventModel.setForceReview(false);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void removeDeletedProduct_rejectedProduct() {
    productChangeEventModel.setProductChangeEventType(
        Collections.singletonList(Constants.PRODUCT_REJECTED));
    productChangeEventModel.setMarkForDelete(true);
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    productOptimisationService.removeDeletedProduct(productChangeEventModel);
    Mockito.verify(productOptimisationRepository).findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
    Mockito.verify(kafkaPublisher).send(EVENT, SELLER_CODE, SELLER_CODE);
  }

  @Test
  void fetchProductOptimisationListTest() throws JsonProcessingException {
    ProductOptimisationListRequest productOptimisationListRequest =
        new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    suggestionImpactDto.setImpact(IMPACT);
    systemParameter.setValue(suggestionImpactDto.toString());
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationDetails> productOptimisationDetailsPage =
        new PageImpl<>(List.of(productOptimisationDetails), pageable, SIZE);
    Mockito.when(
        productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(STORE_ID,
            productOptimisationListRequest, pageable)).thenReturn(productOptimisationDetailsPage);
    Mockito.when(
        systemParameterRepository.findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE))
      .thenReturn(systemParameter);
    Mockito.when(objectMapper.readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class))
      .thenReturn(suggestionImpactDto);
    Page<ProductOptimisationListResponse> responses =
        productOptimisationService.fetchProductOptimisationList(STORE_ID,
            productOptimisationListRequest, PAGE, SIZE);
    Assertions.assertEquals(1, responses.getContent().size());
    Assertions.assertEquals(IMPACT,
      responses.getContent().get(0).getSuggestions().get(0).getImpact());
    Assertions.assertEquals(IMAGE_URL, responses.getContent().getFirst().getImage());
    Mockito.verify(productOptimisationRepository)
        .fetchProductOptimisationListWithFilterApplied(STORE_ID, productOptimisationListRequest,
            pageable);
    Mockito.verify(systemParameterRepository)
      .findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE);
    Mockito.verify(objectMapper)
      .readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class);
  }

  @Test
  void fetchProductOptimisationSuggestionListTest() throws JsonProcessingException {
    ProductOptimisationListRequest productOptimisationListRequest =
      new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    suggestionImpactDto.setImpact(IMPACT);
    systemParameter.setValue(suggestionImpactDto.toString());
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationDetails> productOptimisationDetailsPage =
      new PageImpl<>(List.of(productOptimisationDetails, productOptimisationDetails), pageable,
        SIZE);
    Mockito.when(
      productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(STORE_ID,
        productOptimisationListRequest, pageable)).thenReturn(productOptimisationDetailsPage);
    Mockito.when(
        systemParameterRepository.findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE))
      .thenReturn(systemParameter);
    Mockito.when(objectMapper.readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class))
      .thenReturn(suggestionImpactDto);
    Page<ProductOptimisationListResponse> responses =
      productOptimisationService.fetchProductOptimisationList(STORE_ID,
        productOptimisationListRequest, PAGE, SIZE);
    Assertions.assertEquals(2, responses.getContent().size());
    Assertions.assertEquals(IMPACT,
      responses.getContent().get(0).getSuggestions().get(0).getImpact());
    Mockito.verify(productOptimisationRepository)
      .fetchProductOptimisationListWithFilterApplied(STORE_ID, productOptimisationListRequest,
        pageable);
    Mockito.verify(systemParameterRepository)
      .findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE);
    Mockito.verify(objectMapper)
      .readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class);
  }

  @Test
  void fetchProductOptimisationListExceptionTest() throws JsonProcessingException {
    ProductOptimisationListRequest productOptimisationListRequest =
      new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationDetails> productOptimisationDetailsPage =
      new PageImpl<>(List.of(productOptimisationDetails), pageable, SIZE);
    Mockito.when(
      productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(STORE_ID,
        productOptimisationListRequest, pageable)).thenReturn(productOptimisationDetailsPage);
    Mockito.when(
        systemParameterRepository.findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE))
      .thenReturn(systemParameter);
    Mockito.doThrow(RuntimeException.class).when(objectMapper)
      .readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class);
    Page<ProductOptimisationListResponse> responses =
      productOptimisationService.fetchProductOptimisationList(STORE_ID,
        productOptimisationListRequest, PAGE, SIZE);
    Assertions.assertEquals(1, responses.getContent().size());
    Mockito.verify(productOptimisationRepository)
      .fetchProductOptimisationListWithFilterApplied(STORE_ID, productOptimisationListRequest,
        pageable);
    Mockito.verify(systemParameterRepository)
      .findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE);
    Mockito.verify(objectMapper)
      .readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class);
  }

  @Test
  void fetchProductOptimisationSystemParameterNullTest() throws JsonProcessingException {
    ProductOptimisationListRequest productOptimisationListRequest =
      new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    suggestionImpactDto.setImpact(IMPACT);
    systemParameter.setValue(suggestionImpactDto.toString());
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationDetails> productOptimisationDetailsPage =
      new PageImpl<>(List.of(productOptimisationDetails), pageable, SIZE);
    Mockito.when(
      productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(STORE_ID,
        productOptimisationListRequest, pageable)).thenReturn(productOptimisationDetailsPage);
    Mockito.when(
        systemParameterRepository.findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE))
      .thenReturn(null);
    Mockito.when(objectMapper.readValue(suggestionImpactDto.toString(), SuggestionImpactDto.class))
      .thenReturn(suggestionImpactDto);
    Page<ProductOptimisationListResponse> responses =
      productOptimisationService.fetchProductOptimisationList(STORE_ID,
        productOptimisationListRequest, PAGE, SIZE);
    Assertions.assertEquals(1, responses.getContent().size());
    Mockito.verify(productOptimisationRepository)
      .fetchProductOptimisationListWithFilterApplied(STORE_ID, productOptimisationListRequest,
        pageable);
    Mockito.verify(systemParameterRepository)
      .findByStoreIdAndVariable(Constants.STORE_ID_VALUE, SUGGESTION_TYPE);
  }

  @Test
  void fetchProductOptimisationListTest_nullSuggestions() {
    productOptimisationDetails.setSuggestions(null);
    ProductOptimisationListRequest productOptimisationListRequest =
        new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<ProductOptimisationDetails> productOptimisationDetailsPage =
        new PageImpl<>(List.of(productOptimisationDetails), pageable, SIZE);
    Mockito.when(
        productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(STORE_ID,
            productOptimisationListRequest, pageable)).thenReturn(productOptimisationDetailsPage);
    Page<ProductOptimisationListResponse> responses =
        productOptimisationService.fetchProductOptimisationList(STORE_ID,
            productOptimisationListRequest, PAGE, SIZE);
    Assertions.assertEquals(1, responses.getContent().size());
    Mockito.verify(productOptimisationRepository)
        .fetchProductOptimisationListWithFilterApplied(STORE_ID, productOptimisationListRequest,
            pageable);
  }

  @Test
  void upsertProductOptimisationData_NoExistingDataTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
      .thenReturn(null);
    productOptimisationService.upsertProductOptimisationData(eventModel);
    Mockito.verify(productOptimisationRepository)
      .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository)
      .save(productOptimisationDetailsArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU,
      productOptimisationDetailsArgumentCaptor.getValue().getProductSku());
  }

  @Test
  void upsertProductOptimisationData_WithExistingDataTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
      .thenReturn(productOptimisationDetails);
    productOptimisationService.upsertProductOptimisationData(eventModel);
    Mockito.verify(productOptimisationRepository)
      .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository)
      .save(productOptimisationDetailsArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU,
      productOptimisationDetailsArgumentCaptor.getValue().getProductSku());
  }

  @Test
  void upsertProductOptimisationData_MakeExistingMfdTrueTest() {
    eventModel.setMarkForDelete(true);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
      .thenReturn(productOptimisationDetails);
    productOptimisationService.upsertProductOptimisationData(eventModel);
    Mockito.verify(productOptimisationRepository)
      .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimisationRepository)
      .save(productOptimisationDetailsArgumentCaptor.capture());
    Assertions.assertTrue(productOptimisationDetailsArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  void submitSuggestionFeedback() {
    productOptimisationFeedbackRequest.getSuggestionFeedback().getFirst().setFeedbackType(false);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(
            productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimiseFeedback);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(productOptimiseFeedbackRepository).save(productOptimiseFeedback);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimiseFeedbackRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void submitSuggestionFeedbackEmptyTest() {
    productOptimisationFeedbackRequest.setSuggestionFeedback(null);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository, Mockito.times(0))
        .findByProductSkuAndMarkForDelete(productOptimisationFeedbackRequest.getProductSku(), false);
    Mockito.verify(productOptimiseFeedbackRepository, Mockito.times(0))
        .findByProductSkuAndMarkForDelete(productOptimisationFeedbackRequest.getProductSku(), false);
    Mockito.verify(productOptimiseFeedbackRepository, Mockito.times(0))
        .save(any(ProductOptimiseFeedback.class));
    Mockito.verify(productOptimisationRepository, Mockito.times(0))
        .save(any(ProductOptimisationDetails.class));
  }

  @Test
  void submitSuggestionFeedbackDetailNullTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void submitSuggestionFeedbackNullTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(
            productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(productOptimiseFeedbackRepository).save(productOptimiseFeedback);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimiseFeedbackRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void submitSuggestionFeedbackTest() {

    //positive feedback
    SuggestionFeedbackRequest positiveFeedback = new SuggestionFeedbackRequest();
    positiveFeedback.setFeedbackType(false);
    positiveFeedback.setSuggestionName(SUGGESTION_TYPE);
    List<SuggestionFeedbackRequest> feedbackRequestList = new ArrayList<>();
    feedbackRequestList.add(positiveFeedback);

    //negative feedback
    SuggestionFeedbackRequest negativeFeedbackRequest = new SuggestionFeedbackRequest();
    negativeFeedbackRequest.setFeedbackType(true);
    negativeFeedbackRequest.setSuggestionName(SUGGESTION_TYPE);
    feedbackRequestList.add(negativeFeedbackRequest);

    productOptimisationFeedbackRequest.setSuggestionFeedback(feedbackRequestList);

    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
      .thenReturn(productOptimisationDetails);
    Mockito.when(
        productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
      .thenReturn(null);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository).save(Mockito.any(ProductOptimisationDetails.class));
    Mockito.verify(productOptimiseFeedbackRepository).save(Mockito.any(ProductOptimiseFeedback.class));
    Mockito.verify(productOptimisationRepository)
      .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimiseFeedbackRepository)
      .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void submitSuggestionFeedbackSuggestionNullTest() {
    productOptimisationDetails.setSuggestions(null);
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    Mockito.when(
            productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimiseFeedback);
    productOptimisationService.submitSuggestionFeedback(productOptimisationFeedbackRequest);
    Mockito.verify(productOptimisationRepository).save(productOptimisationDetails);
    Mockito.verify(productOptimiseFeedbackRepository).save(productOptimiseFeedback);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
    Mockito.verify(productOptimiseFeedbackRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void showSuggestionFeedbackTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(productOptimisationDetails);
    productOptimisationService.showSuggestionDetails(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }

  @Test
  void showSuggestionFeedbackNullTest() {
    Mockito.when(productOptimisationRepository.findByProductSkuAndMarkForDelete(PRODUCT_SKU, false))
        .thenReturn(null);
    productOptimisationService.showSuggestionDetails(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productOptimisationRepository)
        .findByProductSkuAndMarkForDelete(PRODUCT_SKU, false);
  }
}
