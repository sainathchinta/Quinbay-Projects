package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;

public class ActivateWorkflowWorkerTest {

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductMailEventService productMailEventService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @InjectMocks
  private ActivateWorkflowWorkerBean activateWorkflowWorkerBean;

  private ProductCollection productCollection;

  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String STORE_ID = "storeId";

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productLevel1WipService).activate(Mockito.any());
    Mockito.doNothing().when(this.productService).createProductLevel3(Mockito.any(), Mockito.eq(false));
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create((String) Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.doNothing().when(this.productMailEventService).createAndSaveMailEvent(Mockito.any() ,
        Mockito.any() ,Mockito.any());
    productCollection = new ProductCollection();
    productCollection.setPostLive(false);
    Mockito.when(
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any())).thenReturn(productCollection);
  }

  Map<String , Object> generateData(){
    Map<String , Object> datas = new HashMap<>();
    datas.put(PRODUCT_CODE ,PRODUCT_CODE);
    datas.put(PROCESS_CODE , PROCESS_CODE);
    datas.put(Constants.STORE_ID, STORE_ID);
    return datas;
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
  }

  @Test
  public void processTest() throws Exception {
    this.activateWorkflowWorkerBean.process(generateData());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productLevel1WipService).activate(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.productLevel1HistoryService).create(Mockito.eq(PRODUCT_CODE) ,
        Mockito.eq(PROCESS_CODE), Mockito.any());
    Mockito.verify(this.productService).createProductLevel3(Mockito.eq(PRODUCT_CODE), Mockito.eq(false));
    Mockito.verify(this.productMailEventService).createAndSaveMailEvent(Mockito.eq(PRODUCT_CODE) ,
        Mockito.any() , Mockito.eq(ProductMailEventsEnum.APPROVED));
  }

  @Test
  public void processPostLiveReviewPendingTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setReviewPending(true);
    this.activateWorkflowWorkerBean.process(generateData());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productLevel1WipService).activate(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.productLevel1HistoryService).create(Mockito.eq(PRODUCT_CODE) ,
        Mockito.eq(PROCESS_CODE), Mockito.any());
    Mockito.verify(this.productService).createProductLevel3(Mockito.eq(PRODUCT_CODE), Mockito.eq(false));
    Mockito.verify(this.productMailEventService).createAndSaveMailEvent(Mockito.eq(PRODUCT_CODE) ,
        Mockito.any() , Mockito.eq(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED));
  }

  @Test
  public void processPostLiveReviewDoneTest() throws Exception {
    productCollection.setPostLive(true);
    productCollection.setReviewPending(false);
    this.activateWorkflowWorkerBean.process(generateData());
    Mockito.verify(productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.productLevel1WipService).activate(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.productLevel1HistoryService).create(Mockito.eq(PRODUCT_CODE) ,
        Mockito.eq(PROCESS_CODE), Mockito.any());
  }

}
