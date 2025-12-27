package com.gdn.partners.product.analytics.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.client.XProduct.XProductOutbound;
import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;
import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.TerminatedSellerDeletionRepository;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import model.TerminatedSellerDeletionEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.FAILED;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.PBP;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.PCB;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.PDT;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.SUCCESS;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.X_PRODUCT;
import static org.mockito.ArgumentMatchers.any;

@ExtendWith(MockitoExtension.class)
public class TerminatedSellerDeletionServiceImplTest {

  @InjectMocks
  private TerminatedSellerDeletionServiceImpl terminatedSellerDeletionService;

  @Mock
  private TerminatedSellerDeletionRepository terminatedSellerDeletionRepository;

  @Mock
  private KafkaProducerService kafkaProducerService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private XProductOutbound xProductOutbound;

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_CODE2 = "productCode2";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU2 = "productSku2";
  private static final String SELLER_CODE = "sellerCode";
  private static final String PDT_TOPIC = "pdt.topic";
  private static final String PBP_TOPIC = "pbp.topic";
  private static final String PCB_TOPIC = "pcb.topic";
  private static final String XPRODUCT_TOPIC = "xproduct.topic";
  public static final String RESULT = "result";
  private static final Set<String> statusList =
      Set.of(TerminatedSellerDeletionConstants.PENDING, TerminatedSellerDeletionConstants.FAILED);
  private static final Set<String> agpStatusList = Set.of("PENDING_AGP");
  private static Map<String, String> serviceNameToTopicMap;
  private static final String AGP_EVENT = "agpEvent";

  private TerminatedSellerDeletion terminatedSellerDeletion;
  private TerminatedSellerDeletion terminatedSellerDeletion2;
  private TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel;
  private TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel2;
  private List<TerminatedSellerDeletion> terminatedSellerDeletionList;
  private Set<String> successStatusesToConsideredForSellerTermination = new HashSet<>();

  @BeforeEach
  public void setup() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
      "statusToFetchForAGPProductDeletion", agpStatusList);
    terminatedSellerDeletion =
        TerminatedSellerDeletion.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU)
            .sellerCode(SELLER_CODE)
            .xProduct(TerminatedSellerDeletionConstants.PENDING)
            .pbp(TerminatedSellerDeletionConstants.PENDING)
            .pdt(TerminatedSellerDeletionConstants.FAILED)
            .pcb(TerminatedSellerDeletionConstants.SUCCESS)
            .finalResult(TerminatedSellerDeletionConstants.FAILED).build();
    terminatedSellerDeletion2 =
        TerminatedSellerDeletion.builder().productCode(PRODUCT_CODE2).productSku(PRODUCT_SKU2)
            .sellerCode(SELLER_CODE)
            .xProduct(TerminatedSellerDeletionConstants.PENDING)
            .pbp(TerminatedSellerDeletionConstants.PENDING)
            .pdt(TerminatedSellerDeletionConstants.PENDING)
            .pcb(TerminatedSellerDeletionConstants.PENDING)
            .finalResult(TerminatedSellerDeletionConstants.PENDING).build();
    terminatedSellerDeletionEventModel =
        TerminatedSellerDeletionEventModel.builder().productCode(PRODUCT_CODE)
            .productSku(PRODUCT_SKU)
            .sellerCode(SELLER_CODE).build();
    terminatedSellerDeletionEventModel2 =
        TerminatedSellerDeletionEventModel.builder().productCode(PRODUCT_CODE2)
            .productSku(PRODUCT_SKU2)
            .sellerCode(SELLER_CODE).build();
    terminatedSellerDeletionList =
        Arrays.asList(terminatedSellerDeletion, terminatedSellerDeletion2);
    successStatusesToConsideredForSellerTermination.add(SUCCESS);
    serviceNameToTopicMap = new HashMap<>();
    serviceNameToTopicMap.put(PBP, PBP_TOPIC);
    serviceNameToTopicMap.put(PCB, PCB_TOPIC);
    serviceNameToTopicMap.put(PDT, PDT_TOPIC);
    serviceNameToTopicMap.put(X_PRODUCT, XPRODUCT_TOPIC);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(terminatedSellerDeletionRepository);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
    Mockito.verifyNoMoreInteractions(kafkaProducerService);
  }

  @Test
  void publishEventsForProductDeletionTest() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            statusList)).thenReturn(terminatedSellerDeletionList);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE)).thenReturn(false);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE2)).thenReturn(false);
    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductEventsMappedToService())
        .thenReturn(serviceNameToTopicMap);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, false);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, statusList);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            serviceNameToTopicMap.get(PDT));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            serviceNameToTopicMap.get(PBP));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            serviceNameToTopicMap.get(X_PRODUCT));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            serviceNameToTopicMap.get(PDT));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            serviceNameToTopicMap.get(PBP));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            serviceNameToTopicMap.get(X_PRODUCT));
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            serviceNameToTopicMap.get(PCB));
    terminatedSellerDeletion.setXProduct(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setPbp(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setPdt(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setRetryCount(1);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setXProduct(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion2.setPbp(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion2.setPdt(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion2.setPcb(TerminatedSellerDeletionConstants.PUBLISHED);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void publishEventsForProductDeletionSharedProductTest() {
    Mockito.when(terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID, statusList))
        .thenReturn(terminatedSellerDeletionList);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE)).thenReturn(true);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE2)).thenReturn(true);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, false);
    Mockito.verify(terminatedSellerDeletionRepository).fetchTerminatedSellerProductsForDeletion(STORE_ID, statusList);
    Mockito.verify(xProductOutbound).isSharedProduct(SELLER_CODE, PRODUCT_CODE);
    Mockito.verify(xProductOutbound).isSharedProduct(SELLER_CODE, PRODUCT_CODE2);
    terminatedSellerDeletion.setSkipped(true);
    terminatedSellerDeletion.setFinalResult(TerminatedSellerDeletionConstants.SUCCESS);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setSkipped(true);
    terminatedSellerDeletion2.setFinalResult(TerminatedSellerDeletionConstants.SUCCESS);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    terminatedSellerDeletionEventModel.setSharedProduct(true);
    terminatedSellerDeletionEventModel2.setSharedProduct(true);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void publishEventsForProductDeletionTest_exceptionTest() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            statusList)).thenReturn(Collections.singletonList(terminatedSellerDeletion));
    Mockito.when(terminatedSellerDeletionRepository.save(terminatedSellerDeletion))
      .thenReturn(terminatedSellerDeletion);
    Mockito.doThrow(new ApplicationRuntimeException()).when(xProductOutbound)
        .isSharedProduct(SELLER_CODE, PRODUCT_CODE);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, false);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, statusList);
    terminatedSellerDeletion.setXProduct(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setPbp(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setPdt(TerminatedSellerDeletionConstants.PUBLISHED);
    terminatedSellerDeletion.setRetryCount(1);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
   void publishEventsForProductDeletionTest_emptyList() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            statusList)).thenReturn(new ArrayList<>());
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, false);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, statusList);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void publishEventsForProductDeletionTest_publishForAGPDeletionTrue() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            agpStatusList)).thenReturn(terminatedSellerDeletionList);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE)).thenReturn(false);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE2)).thenReturn(false);
    Mockito.when(kafkaTopicProperties.getAgpPermanentDeleteProductEventName())
        .thenReturn(AGP_EVENT);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, true);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, agpStatusList);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            AGP_EVENT);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            AGP_EVENT);

    terminatedSellerDeletion.setFinalResult(SUCCESS);
    terminatedSellerDeletion.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setFinalResult(SUCCESS);
    terminatedSellerDeletion2.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }


  @Test
  void publishEventsForProductDeletionTest_publishForAGPDeletionTrueForShareProduct() {
    terminatedSellerDeletionList.forEach(terminatedSellerDeletion1 -> terminatedSellerDeletion1.setSharedProduct(false));
    Mockito.when(
      terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
        agpStatusList)).thenReturn(terminatedSellerDeletionList);
    Mockito.when(kafkaTopicProperties.getAgpPermanentDeleteProductEventName())
      .thenReturn(AGP_EVENT);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, true);
    Mockito.verify(terminatedSellerDeletionRepository)
      .fetchTerminatedSellerProductsForDeletion(STORE_ID, agpStatusList);
    Mockito.verify(kafkaProducerService)
      .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
        AGP_EVENT);
    Mockito.verify(kafkaProducerService)
      .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
        AGP_EVENT);

    terminatedSellerDeletion.setFinalResult(SUCCESS);
    terminatedSellerDeletion.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setFinalResult(SUCCESS);
    terminatedSellerDeletion2.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void publishEventsForProductDeletionTest_publishForAGPDeletionTrueEmptyList() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            agpStatusList)).thenReturn(Collections.emptyList());
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, true);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, agpStatusList);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void publishEventsForProductDeletionTest_publishForAGPDeletionTrueSharedProduct() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            agpStatusList)).thenReturn(terminatedSellerDeletionList);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE)).thenReturn(true);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE2)).thenReturn(false);
    Mockito.when(kafkaTopicProperties.getAgpPermanentDeleteProductEventName())
        .thenReturn(AGP_EVENT);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, true);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, agpStatusList);
    terminatedSellerDeletionEventModel.setSharedProduct(true);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            AGP_EVENT);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel2,
            AGP_EVENT);

    terminatedSellerDeletion.setFinalResult(SUCCESS);
    terminatedSellerDeletion.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setFinalResult(SUCCESS);
    terminatedSellerDeletion2.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());

  }

  @Test
  void publishEventsForProductDeletionTest_publishForAGPDeletionTrueException() {
    Mockito.when(
        terminatedSellerDeletionRepository.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            agpStatusList)).thenReturn(terminatedSellerDeletionList);
    Mockito.when(xProductOutbound.isSharedProduct(SELLER_CODE, PRODUCT_CODE)).thenReturn(false);
    Mockito.doThrow(new ApplicationRuntimeException()).when(xProductOutbound)
        .isSharedProduct(SELLER_CODE, PRODUCT_CODE2);
    Mockito.when(kafkaTopicProperties.getAgpPermanentDeleteProductEventName())
        .thenReturn(AGP_EVENT);
    terminatedSellerDeletionService.publishEventsForProductDeletion(STORE_ID, true);
    Mockito.verify(terminatedSellerDeletionRepository)
        .fetchTerminatedSellerProductsForDeletion(STORE_ID, agpStatusList);
    Mockito.verify(kafkaProducerService)
        .publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
            AGP_EVENT);

    terminatedSellerDeletion.setFinalResult(SUCCESS);
    terminatedSellerDeletion.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion);
    terminatedSellerDeletion2.setFinalResult(FAILED);
    terminatedSellerDeletion2.setMarkForDelete(true);
    Mockito.verify(terminatedSellerDeletionRepository).save(terminatedSellerDeletion2);
    Mockito.verify(terminatedSellerDeletionRepository).saveAll(any());
  }

  @Test
  void testUpdateStatusInRespectiveService_PBP() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
        "successStatusesForSellerTermination", successStatusesToConsideredForSellerTermination);
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    terminatedSellerDeletion.setPcb(SUCCESS);
    terminatedSellerDeletion.setPdt(SUCCESS);
    terminatedSellerDeletion.setXProduct(SUCCESS);
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        PBP, SUCCESS);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
    Mockito.verify(terminatedSellerDeletionRepository).save(any(TerminatedSellerDeletion.class));
  }

  @Test
  void testUpdateStatusInRespectiveService_PDT() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
        "successStatusesForSellerTermination", successStatusesToConsideredForSellerTermination);
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    terminatedSellerDeletion.setPbp(SUCCESS);
    terminatedSellerDeletion.setPcb(SUCCESS);
    terminatedSellerDeletion.setXProduct(FAILED);
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        PDT, SUCCESS);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
    Mockito.verify(terminatedSellerDeletionRepository).save(any(TerminatedSellerDeletion.class));
  }

  @Test
  void testUpdateStatusInRespectiveService_PCB() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
        "successStatusesForSellerTermination", successStatusesToConsideredForSellerTermination);
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    terminatedSellerDeletion.setPbp(FAILED);
    terminatedSellerDeletion.setXProduct(SUCCESS);
    terminatedSellerDeletion.setPdt(SUCCESS);
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        PCB, SUCCESS);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
    Mockito.verify(terminatedSellerDeletionRepository).save(any(TerminatedSellerDeletion.class));
  }

  @Test
  void testUpdateStatusInRespectiveService_XProduct() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
        "successStatusesForSellerTermination", successStatusesToConsideredForSellerTermination);
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    terminatedSellerDeletion.setPbp(SUCCESS);
    terminatedSellerDeletion.setPcb(FAILED);
    terminatedSellerDeletion.setPdt(SUCCESS);
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        X_PRODUCT, SUCCESS);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
    Mockito.verify(terminatedSellerDeletionRepository).save(any(TerminatedSellerDeletion.class));
  }

  @Test
  void testUpdateStatusInRespectiveService_UnknownService() {
    String service = "unknown";
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        service, RESULT);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
  }

  @Test
  void testUpdateStatusInRespectiveServiceFailureInOne() {
    ReflectionTestUtils.setField(terminatedSellerDeletionService,
        "successStatusesForSellerTermination", successStatusesToConsideredForSellerTermination);
    Set<String> statusesToBeIgnored = Set.of("PENDING_AGP", "SUCCESS");
    TerminatedSellerDeletion terminatedSellerDeletion = new TerminatedSellerDeletion();
    terminatedSellerDeletion.setPbp(SUCCESS);
    terminatedSellerDeletion.setXProduct(SUCCESS);
    terminatedSellerDeletion.setPdt(FAILED);
    Mockito.when(terminatedSellerDeletionRepository.findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE,
        SELLER_CODE, statusesToBeIgnored)).thenReturn(terminatedSellerDeletion);

    terminatedSellerDeletionService.updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE,
        PCB, SUCCESS);

    Mockito.verify(terminatedSellerDeletionRepository)
        .findByProductCodeAndSellerCodeAndFinalResultNotIn(PRODUCT_CODE, SELLER_CODE, statusesToBeIgnored);
    Mockito.verify(terminatedSellerDeletionRepository).save(any(TerminatedSellerDeletion.class));
  }
}
