package com.gdn.x.mta.distributiontask.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.base.domainevent.subscriber.KafkaMessageInterceptor;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;

@Component
public class ProductKafkaMessageInterceptor implements KafkaMessageInterceptor {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public List<GdnBaseDomainEventModel> interceptMessage(String topicName,
      List<GdnBaseDomainEventModel> messages) {
    if (kafkaTopicProperties.getProductScreeningApprovedTaskEvent().equals(topicName)) {
      Map<String, GdnBaseDomainEventModel> resultMap =
          new HashMap<String, GdnBaseDomainEventModel>();
      for (GdnBaseDomainEventModel model : messages) {
        PDTProductDomainEventModel productChange = (PDTProductDomainEventModel) model;
        String productCode = productChange.getProductCode();
        PDTProductDomainEventModel registeredModel =
            (PDTProductDomainEventModel) resultMap.get(productCode);
        if (registeredModel != null) {
          if (registeredModel.getTimestamp() < productChange.getTimestamp()) {
            resultMap.remove(productCode);
            resultMap.put(productCode, productChange);
          }
        } else {
          resultMap.put(productCode, productChange);
        }
      }
      return new ArrayList<GdnBaseDomainEventModel>(resultMap.values());
    } else if (DomainEventName.PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME
        .equals(topicName)) {
      Map<String, GdnBaseDomainEventModel> resultMap =
          new HashMap<String, GdnBaseDomainEventModel>();
      for (GdnBaseDomainEventModel model : messages) {
        ProductForcedRollbackSLAExceedDomainEventModel productChange =
            (ProductForcedRollbackSLAExceedDomainEventModel) model;
        String productCode = productChange.getProductCode();
        ProductForcedRollbackSLAExceedDomainEventModel registeredModel =
            (ProductForcedRollbackSLAExceedDomainEventModel) resultMap.get(productCode);
        if (registeredModel != null) {
          if (registeredModel.getTimestamp() < productChange.getTimestamp()) {
            resultMap.remove(productCode);
            resultMap.put(productCode, productChange);
          }
        } else {
          resultMap.put(productCode, productChange);
        }
      }
      return new ArrayList<GdnBaseDomainEventModel>(resultMap.values());
    } else {
      return messages;
    }
  }

}
