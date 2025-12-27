package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * Created by govind on 06/02/2019 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BulkProcessServiceImplTest {

  private static final String ASSIGNED_TO = "assignedTo";
  private static final String SEARCH_KEYWORD = "searchKeyword";
  private static final String SORT_COLUMN = "sortColumn";
  private static final String SORT_ORDER = "asc";
  private static final String STATUS_FILTER = "statusFilter";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";
  private static final String LANGUAGE = "language";


  @InjectMocks
  private BulkProcessServiceImpl bulkProcessService;

  @Mock
  private KafkaPublisher kafkaProducer;

  private ReviewProductsFilterRequest reviewProductsFilterRequest;

  @BeforeEach
  public void setUp() throws Exception {

    reviewProductsFilterRequest =
        ReviewProductsFilterRequest.builder().assignedTo(ASSIGNED_TO).categoryCode(CATEGORY_CODE)
            .searchKeyword(SEARCH_KEYWORD).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .sortColumn(SORT_COLUMN).sortOrder(SORT_ORDER).statusFilter(STATUS_FILTER)
            .timeFilter(TIME_FILTER).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.kafkaProducer);
  }

  @Test
  public void bulkDownloadScreeningProductsTest(){
    this.bulkProcessService.bulkDownloadScreeningProducts(Constants.USER_NAME,
        reviewProductsFilterRequest, LANGUAGE);
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            Mockito.any(BulkDownloadRequest.class));
  }

}
