package com.gdn.x.product.service.aspect;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.concurrent.ExecutorService;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.task.SolrIndexerTask;

public class SolrIndexerAspectTest {

  @InjectMocks
  private SolrIndexerAspect aspect;

  @Mock
  private ExecutorService executorService;

  @Mock
  private ProductAndItemSolrIndexerService indexerService;

  @Test
  public void indexSolrExceptionOccurs() {
    Item item = new Item();
    doThrow(RuntimeException.class).when(this.executorService).submit(
        new SolrIndexerTask(this.indexerService, item));
    this.aspect.indexSolr(item);
    verify(this.executorService).submit(new SolrIndexerTask(this.indexerService, item));
  }

  @Test
  public void indexSolrSuccess() {
    Item item = new Item();
    this.aspect.indexSolr(item);
    verify(this.executorService).submit(new SolrIndexerTask(this.indexerService, item));
  }

  @BeforeEach
  public void init() {
    openMocks(this);
  }


  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.executorService);
    verifyNoMoreInteractions(this.indexerService);
  }
}
