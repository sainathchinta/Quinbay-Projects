package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.convert.MongoConverter;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
class SellerAnalyticsRepositoryImplTest {

  @InjectMocks
  private SellerAnalyticsRepositoryImpl sellerAnalyticsRepository;
  @Mock
  private MongoTemplate mongoTemplate;

  @Mock
  private MongoConverter mongoConverter;

  private static final String SELLER_CODE = "sellerCode";


  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(mongoTemplate);
  }

  @Test
  void bulkWriteSellerAnalyticsTest() {
    SellerAnalytics sellerAnalytics = new SellerAnalytics();
    sellerAnalytics.setSellerCode(SELLER_CODE);
    List<SellerAnalytics> sellerAnalyticsList = new ArrayList<>();
    sellerAnalyticsList.add(sellerAnalytics);
    BulkOperations bulkOperations = mock(BulkOperations.class);
    Mockito.when(mongoTemplate.bulkOps(Mockito.eq(BulkOperations.BulkMode.UNORDERED),
      Mockito.eq(SellerAnalytics.COLLECTION_NAME))).thenReturn(bulkOperations);
    Mockito.when(mongoTemplate.getConverter()).thenReturn(mongoConverter);
    Mockito.when(bulkOperations.execute()).thenReturn(null);
    sellerAnalyticsRepository.bulkWriteSellerAnalyticsDetail(sellerAnalyticsList);
    Mockito.verify(mongoTemplate).bulkOps(Mockito.eq(BulkOperations.BulkMode.UNORDERED),
      Mockito.eq(SellerAnalytics.COLLECTION_NAME));
    Mockito.verify(mongoTemplate).getConverter();
  }

}
