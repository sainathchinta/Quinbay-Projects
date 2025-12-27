package com.gdn.x.product.service.util;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.outbound.api.ProductSolrOutbound;

public class SolrOptimizeServiceImplTest {

  @InjectMocks
  private SolrOptimizeServiceImpl solrOptimizeServiceImpl;

  @Mock
  private ProductSolrOutbound productSolrOutbound;

  @BeforeEach
  public void before() {
    openMocks(this);
  }

  @Test
  public void optimize() {
    this.solrOptimizeServiceImpl.optimize();
    verify(this.productSolrOutbound).optimize();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productSolrOutbound);
  }

}
