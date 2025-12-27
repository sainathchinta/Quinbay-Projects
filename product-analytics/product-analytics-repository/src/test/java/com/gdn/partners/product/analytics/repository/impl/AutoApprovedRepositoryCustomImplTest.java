package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.convert.MongoConverter;
import org.springframework.data.mongodb.core.query.Query;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.mock;

public class AutoApprovedRepositoryCustomImplTest {

  private static final int PAGE = 0;
  private static final int SIZE = 1;
  private static final String KEYWORD = "keyword";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String PRODUCT_CODE = "MTA-12345";
  private static final String DESC = "DESC";

  @InjectMocks
  private AutoApprovedRepositoryCustomImpl autoApprovedRepositoryCustom;
  @Mock
  private MongoTemplate mongoTemplate;

  @Mock
  private MongoConverter mongoConverter;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(mongoTemplate);
  }

  @Test
  public void testFetchListOfAutoApprovedProducts() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setKeyword(KEYWORD);
    request.setCategoryCode(CATEGORY_CODE);
    request.setAssignedTo(ASSIGNED_TO);
    request.setSellerCode(SELLER_CODE);
    request.setSortOrder(DESC);
    request.setB2bActivated(true);
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    AutoApprovedProducts product = new AutoApprovedProducts();
    Page<AutoApprovedProducts> expectedResult = new PageImpl<>(List.of(product), pageable, SIZE);

    Mockito.when(mongoTemplate.count(Mockito.any(Query.class), (Class<?>) Mockito.any()))
      .thenReturn(0L);
    Mockito.when(mongoTemplate.find(Mockito.any(Query.class), Mockito.any()))
      .thenReturn(Collections.singletonList(product));

    Page<AutoApprovedProducts> result =
      autoApprovedRepositoryCustom.fetchListOfAutoApprovedProducts(request, pageable, true);

    Assertions.assertEquals(expectedResult, result);
    Mockito.verify(mongoTemplate).count(Mockito.any(Query.class), (Class<?>) Mockito.any());
    Mockito.verify(mongoTemplate).find(Mockito.any(Query.class), Mockito.any());
  }

  @Test
  public void testFetchListOfAutoApprovedProductsFindProductSku() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setKeyword(PRODUCT_CODE);
    request.setCategoryCode("");
    request.setAssignedTo(Constants.UNASSIGNED_FILTER);
    request.setSellerCode("");
    request.setB2bActivated(null);
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    AutoApprovedProducts product = new AutoApprovedProducts();
    Page<AutoApprovedProducts> expectedResult = new PageImpl<>(List.of(product), pageable, SIZE);

    Mockito.when(mongoTemplate.count(Mockito.any(Query.class), (Class<?>) Mockito.any()))
      .thenReturn(0L);
    Mockito.when(mongoTemplate.find(Mockito.any(Query.class), Mockito.any()))
      .thenReturn(Collections.singletonList(product));

    Page<AutoApprovedProducts> result =
      autoApprovedRepositoryCustom.fetchListOfAutoApprovedProducts(request, pageable, false);

    Assertions.assertEquals(expectedResult, result);
    Mockito.verify(mongoTemplate).count(Mockito.any(Query.class), (Class<?>) Mockito.any());
    Mockito.verify(mongoTemplate).find(Mockito.any(Query.class), Mockito.any());
  }

  @Test
  public void testFetchListOfAutoApprovedProductsEmptyKeyword() {
    AutoApprovedWebRequest request = new AutoApprovedWebRequest();
    request.setKeyword("");
    request.setCategoryCode("");
    request.setAssignedTo("");
    request.setSellerCode("");
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    AutoApprovedProducts product = new AutoApprovedProducts();
    Page<AutoApprovedProducts> expectedResult = new PageImpl<>(List.of(product), pageable, SIZE);

    Mockito.when(mongoTemplate.count(Mockito.any(Query.class), (Class<?>) Mockito.any()))
      .thenReturn(0L);
    Mockito.when(mongoTemplate.find(Mockito.any(Query.class), Mockito.any()))
      .thenReturn(Collections.singletonList(product));

    Page<AutoApprovedProducts> result =
      autoApprovedRepositoryCustom.fetchListOfAutoApprovedProducts(request, pageable, false);

    Assertions.assertEquals(expectedResult, result);
    Mockito.verify(mongoTemplate).count(Mockito.any(Query.class), (Class<?>) Mockito.any());
    Mockito.verify(mongoTemplate).find(Mockito.any(Query.class), Mockito.any());
  }

  @Test
  public void countNumberOfRecordsTest() {
    Mockito.when(mongoTemplate.count(Mockito.any(Query.class), (Class<?>) Mockito.any()))
      .thenReturn(10L);
    Assertions.assertEquals(10L, autoApprovedRepositoryCustom.countNumberOfRecords());
    Mockito.verify(mongoTemplate).count(Mockito.any(Query.class), (Class<?>) Mockito.any());
  }


  @Test
  public void bulkWriteAutoApprovedProductsTest() {
    AutoApprovedProducts autoApprovedProducts = new AutoApprovedProducts();
    autoApprovedProducts.setProductCode(PRODUCT_CODE);
    List<AutoApprovedProducts> autoApprovedProductsList = new ArrayList<>();
    autoApprovedProductsList.add(autoApprovedProducts);
    BulkOperations bulkOperations = mock(BulkOperations.class);
    Mockito.when(mongoTemplate.bulkOps(Mockito.eq(BulkOperations.BulkMode.UNORDERED),
      Mockito.eq(AutoApprovedProducts.COLLECTION_NAME))).thenReturn(bulkOperations);
    Mockito.when(mongoTemplate.getConverter()).thenReturn(mongoConverter);
    Mockito.when(bulkOperations.execute()).thenReturn(null); // Assuming no need to assert
    autoApprovedRepositoryCustom.bulkWriteAutoApprovedProducts(autoApprovedProductsList);
    Mockito.verify(mongoTemplate).bulkOps(Mockito.eq(BulkOperations.BulkMode.UNORDERED),
      Mockito.eq(AutoApprovedProducts.COLLECTION_NAME));
    Mockito.verify(mongoTemplate).getConverter();
  }
}
