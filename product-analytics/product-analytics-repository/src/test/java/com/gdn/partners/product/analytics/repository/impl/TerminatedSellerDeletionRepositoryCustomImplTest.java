package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.FAILED;
import static com.gdn.partners.product.analytics.model.TerminatedSellerDeletionConstants.PENDING;

@ExtendWith(MockitoExtension.class)
public class TerminatedSellerDeletionRepositoryCustomImplTest {

  @InjectMocks
  private TerminatedSellerDeletionRepositoryCustomImpl terminatedSellerDeletionRepositoryCustom;

  @Mock
  private MongoTemplate mongoTemplate;

  private static final String STORE_ID = "10001";
  private static final Set<String> statusList = Set.of(PENDING, FAILED);

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(mongoTemplate);
  }

  @Test
  public void fetchTerminatedSellerProductsForDeletionTest() {
    ReflectionTestUtils.setField(terminatedSellerDeletionRepositoryCustom,
        "terminatedSellerPublishBatchSize", 10);
    ReflectionTestUtils.setField(terminatedSellerDeletionRepositoryCustom,
        "retryCountLimit", 2);
    Mockito.when(mongoTemplate.find(Mockito.any(Query.class), Mockito.any())).thenReturn(new ArrayList<>());
    List<TerminatedSellerDeletion> terminatedSellerDeletionList =
        terminatedSellerDeletionRepositoryCustom.fetchTerminatedSellerProductsForDeletion(STORE_ID,
            statusList);
    Mockito.verify(mongoTemplate).find(Mockito.any(Query.class), Mockito.any());
    Assertions.assertEquals(0, terminatedSellerDeletionList.size());
  }
}
