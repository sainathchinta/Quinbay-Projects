package com.gdn.mta.product.service.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.esotericsoftware.minlog.Log;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.newrelic.api.agent.Trace;

@Service
public class ProductSubscriber {

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;
  
  private ProductService getProductService() {
    return this.productService;
  }

  @Trace(dispatcher=true)
  @KafkaListener(topics = DomainEventName.PRODUCT_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductDomainEventModel product = objectMapper.readValue(message, ProductDomainEventModel.class);
    if(product.isproductMarkForDelete() == true){
      Log.debug("retry delete product with product code: " + product.getProductCode() + ", product id: " + product.getId(), 
          ", markForDelete: " + product.isproductMarkForDelete());
      try{
        this.getProductService().retryDelete(product.getId(), product.getStoreId(), product.getProductCode(), 
            product.getName(), product.isproductMarkForDelete());
      } catch(Exception e){
        Log.error("failed to retry detele product", e);
      }
    }
  }

}
