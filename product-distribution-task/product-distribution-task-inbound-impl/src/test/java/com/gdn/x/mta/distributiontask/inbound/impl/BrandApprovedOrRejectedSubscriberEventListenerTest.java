package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.Mockito.when;

import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;

import java.util.Arrays;

public class BrandApprovedOrRejectedSubscriberEventListenerTest {

  private static final String BRAND_NAME = "brandName";
  private static final String PRODUCT_CODE = "productCode";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";

  private BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel;

  @InjectMocks
  private BrandApprovedOrRejectedSubscriberEventListener brandApprovedOrRejectedSubscriberEventListener;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mapper = new ObjectMapper();
    brandApprovedOrRejectedDomainEventModel = new BrandApprovedOrRejectedDomainEventModel();
    brandApprovedOrRejectedDomainEventModel.setBrandName(BRAND_NAME);
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productWrapperService, objectMapper);
  }

  @Test
   void onDomainEventConsumed() throws Exception {
    String message = mapper.writeValueAsString(brandApprovedOrRejectedDomainEventModel);
    when(objectMapper.readValue(message, BrandApprovedOrRejectedDomainEventModel.class))
        .thenReturn(brandApprovedOrRejectedDomainEventModel);
    Mockito.doNothing().when(this.productWrapperService)
        .updateBrandApprovalStatusAndUpdateSolr(brandApprovedOrRejectedDomainEventModel);
    brandApprovedOrRejectedSubscriberEventListener.onDomainEventConsumed(message);
    Mockito.verify(productWrapperService)
        .updateBrandApprovalStatusAndUpdateSolr(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(objectMapper).readValue(message, BrandApprovedOrRejectedDomainEventModel.class);
  }
}
