package com.gdn.partners.pbp.service.listener;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections.MapUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepository;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.newrelic.api.agent.Trace;

@Service
public class SolrAddReviewProductCollectionKafkaSubscriberBean {

  @Autowired
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SolrReviewProductCollectionRepository solrReviewProductCollectionRepository;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrAddReviewProductCollectionKafkaSubscriberBean.class);

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws IOException, SolrServerException {
    SolrReviewProductCollectionAddEvent solrProductCollectionAddEvent =
        objectMapper.readValue(message, SolrReviewProductCollectionAddEvent.class);
    LOGGER
        .info("[SolrAddReviewProductCollectionKafkaSubscriberBean] " + "Retrieved message from Topic: {}, Message: {}",
            DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrProductCollectionAddEvent);
    if (MapUtils.isNotEmpty(solrProductCollectionAddEvent.getFieldsAndValues())) {
      solrReviewProductCollectionRepository.atomicUpdateToSolr(solrProductCollectionAddEvent.getFieldsAndValues());
    }
    else {
      List<SolrInputDocument> solrInputFields =
          solrProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList().stream().map(this::toSolrInputDocument).collect(Collectors.toList());
      LOGGER.info("SolrInputDocument: {}", solrInputFields);
      cloudSolrClient.add(solrInputFields);
    }
  }

  private SolrInputDocument toSolrInputDocument(
      SolrReviewProductCollectionAddEventFields solrProductCollectionAddEvent) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, solrProductCollectionAddEvent.getId());
    solrInputDocument.setField(SolrFieldNames.STORE_ID,
        solrProductCollectionAddEvent.getStoreId());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_ID,
        solrProductCollectionAddEvent.getProductId());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE,
        solrProductCollectionAddEvent.getProductCode());
    solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
        solrProductCollectionAddEvent.getProductName());
    solrInputDocument.setField(SolrFieldNames.BRAND,
        solrProductCollectionAddEvent.getBrand());
    solrInputDocument.setField(SolrFieldNames.CATEGORY_CODES,
        solrProductCollectionAddEvent.getCategoryCodes());
    solrInputDocument.setField(SolrFieldNames.CATEGORY_NAMES,
        solrProductCollectionAddEvent.getCategoryNames());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrProductCollectionAddEvent.getBusinessPartnerCode());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_NAME,
        solrProductCollectionAddEvent.getBusinessPartnerName());
    solrInputDocument.setField(SolrFieldNames.UPDATED_STEP_DATE,
        solrProductCollectionAddEvent.getUpdatedStepDate());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        solrProductCollectionAddEvent.getUpdatedDate());
    solrInputDocument.setField(SolrFieldNames.CREATED_DATE,
        solrProductCollectionAddEvent.getCreatedDate());
    solrInputDocument.setField(SolrFieldNames.CREATED_BY,
        solrProductCollectionAddEvent.getCreatedBy());
    solrInputDocument.setField(SolrFieldNames.UPDATED_BY,
        solrProductCollectionAddEvent.getUpdatedBy());
    solrInputDocument.setField(SolrFieldNames.ASSIGNED_TO,
        solrProductCollectionAddEvent.getAssignedTo());
    solrInputDocument.setField(SolrFieldNames.ACTIVATED,
        solrProductCollectionAddEvent.isActivated());
    solrInputDocument.setField(SolrFieldNames.VIEWABLE,
        solrProductCollectionAddEvent.isViewable());
    solrInputDocument.setField(SolrFieldNames.RESUBMIT_COUNT,
        solrProductCollectionAddEvent.getResubmitCount());
    solrInputDocument.setField(SolrFieldNames.SUBMITTED_DATE,
        solrProductCollectionAddEvent.getSubmittedDate());
    solrInputDocument.setField(SolrFieldNames.STATE,
        solrProductCollectionAddEvent.getState());
    solrInputDocument.setField(SolrFieldNames.BRAND_APPROVED,
        solrProductCollectionAddEvent.isBrandApproved());
    return solrInputDocument;
  }
}