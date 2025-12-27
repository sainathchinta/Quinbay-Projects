package com.gdn.x.productcategorybase.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.productcategorybase.AggregateCommandDesc;
import com.gdn.x.productcategorybase.service.AggregateService;
import com.gdn.x.productcategorybase.service.AsyncProcessor;

public class AggregateControllerTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final int START_PAGE = 0;

  @Mock
  private AggregateService aggregateService;

  @Mock
  private AsyncProcessor asyncProcessor;

  @InjectMocks
  private AggregateController aggregateController;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @Test
  public void testPublishAllProducts() throws Exception {
    this.aggregateController.publishAllProducts(STORE_ID,CHANNEL_ID,CLIENT_ID,REQUEST_ID,DEFAULT_USERNAME,START_PAGE);
    verify(asyncProcessor,times(1))
        .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_CONTROLLER),any(Runnable.class));
  }

  @Test
  public void testPublishAllProductCategories() throws Exception {
    this.aggregateController.publishAllProductCategories(STORE_ID,CHANNEL_ID,CLIENT_ID,REQUEST_ID,DEFAULT_USERNAME,START_PAGE);
    verify(asyncProcessor,times(1))
        .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_CATEGORY_CONTROLLER),any(Runnable.class));
  }

  @Test
  public void testPublishAllProductAttributes() throws Exception {
    this.aggregateController.publishAllProductAttributes(STORE_ID,CHANNEL_ID,CLIENT_ID,REQUEST_ID,DEFAULT_USERNAME,START_PAGE);
    verify(asyncProcessor,times(1))
        .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_ATTRIBUTE_CONTROLLER),any(Runnable.class));
  }

  @Test
  public void testPublishAllImages() throws Exception {
    this.aggregateController.publishAllImages(STORE_ID,CHANNEL_ID,CLIENT_ID,REQUEST_ID,DEFAULT_USERNAME,START_PAGE);
    verify(asyncProcessor,times(1))
        .submitWithBackoff(eq(AggregateCommandDesc.IMAGE_CONTROLLER),any(Runnable.class));
  }

  @Test
  public void testPublishAllProductItems() throws Exception {
    this.aggregateController.publishAllProductItems(STORE_ID,CHANNEL_ID,CLIENT_ID,REQUEST_ID,DEFAULT_USERNAME,START_PAGE);
    verify(asyncProcessor,times(1))
        .submitWithBackoff(eq(AggregateCommandDesc.PRODUCT_ITEM_CONTROLLER),any(Runnable.class));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.aggregateService);
    verifyNoMoreInteractions(this.asyncProcessor);
  }

}
