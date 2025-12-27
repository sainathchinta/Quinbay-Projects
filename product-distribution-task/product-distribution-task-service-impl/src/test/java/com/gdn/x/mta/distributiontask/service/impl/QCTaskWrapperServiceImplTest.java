package com.gdn.x.mta.distributiontask.service.impl;


import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.QCTaskService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class QCTaskWrapperServiceImplTest {

  private static final String REASON = "reason";
  private static final String PRODUCT_CODE = "productCode";

  private final Product product = new Product();

  @Mock
  private QCTaskService qcTaskService;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @InjectMocks
  private QCTaskWrapperServiceImpl qcTaskWrapperServiceImpl;

  @BeforeEach
  public void setUp() throws Exception {
    product.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(this.qcTaskService);
  }

  @Test
   void qcRejectProductTest_assignedToVendorFalse() throws Exception {
    qcTaskWrapperServiceImpl.qcRejectProduct(product, false, REASON, WorkflowState.QC_REJECTED);
    Mockito.verify(this.qcTaskService)
        .qcRejectProduct(product, false, REASON, WorkflowState.QC_REJECTED);
    Mockito.verify(this.solrVendorCollectionService)
        .updateStateOnSolr(PRODUCT_CODE, WorkflowState.UNASSIGNED);
  }

  @Test
   void qcRejectProductTest_assignedToVendorTrue() throws Exception {
    qcTaskWrapperServiceImpl.qcRejectProduct(product, true, REASON, WorkflowState.QC_REJECTED);
    Mockito.verify(this.qcTaskService)
        .qcRejectProduct(product, true, REASON, WorkflowState.QC_REJECTED);
    Mockito.verify(this.solrVendorCollectionService)
        .clearReviewerDetailsAndUpdateState(PRODUCT_CODE, WorkflowState.QC_REJECTED);
  }
}
