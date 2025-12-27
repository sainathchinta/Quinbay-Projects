package com.gdn.x.product.service.aspect;

import java.util.concurrent.ExecutorService;

import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.task.SolrIndexerTask;
import org.springframework.beans.factory.annotation.Qualifier;

@Aspect
public class SolrIndexerAspect {

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrIndexerAspect.class);

  @Autowired
  @Qualifier(value = "executorService")
  private ExecutorService executorService;

  @Autowired
  private ProductAndItemSolrIndexerService indexerService;

  @Autowired
  private ProductService productService;

  @AfterReturning(pointcut = "@annotation(com.gdn.x.product.service.annotation.SolrIndex)",
      returning = "returnObject")
  public void indexSolr(Object returnObject) {
    try {
      SolrIndexerTask task = new SolrIndexerTask(this.indexerService, productService, returnObject);
      this.executorService.submit(task);
    } catch (Exception e) {
      SolrIndexerAspect.LOGGER.warn("failed on publishing object to executor {}", returnObject, e);
    }
  }
}
