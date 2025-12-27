package com.gdn.partners.product.analytics.service.impl.helper;

import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import lombok.extern.slf4j.Slf4j;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.StringWriter;

@Slf4j
@Component
public class QueryTemplateHelper {

  @Autowired
  private VelocityEngine velocityEngine;

  @Autowired
  private GCPProperties gcpProperties;

  public String getFullFetchQuery(String jobProcessType, int recordFetchSize,
      int fetchHourThreshold, int suggestedDateFetch, String attributeName,
    int autoApprovalFetchHour) {
    Template template;
    if (JobProcessTypes.SELLER_INFO_BQ_JOB.name().equals(jobProcessType)) {
      template = velocityEngine.getTemplate(Constants.FULL_FETCH_TEMPLATE);
    } else if (JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name().equals(jobProcessType)) {
      template = velocityEngine.getTemplate(Constants.FULL_FETCH_SELLER_SPECIFIC_TEMPLATE);
    } else if(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name().equals(jobProcessType)){
      template = velocityEngine.getTemplate(Constants.AUTO_APPROVED_PRODUCTS_TEMPLATE);
    } else if(JobProcessTypes.IPR_PRODUCTS_JOB.name().equals(jobProcessType)){
      template = velocityEngine.getTemplate(Constants.IPR_PRODUCTS_TEMPLATE);
    } else if (JobProcessTypes.PRODUCT_OPTIMISATION_JOB.name().equals(jobProcessType)) {
      template = velocityEngine.getTemplate(Constants.PRODUCT_OPTIMISATION_TEMPLATE);
    } else if(JobProcessTypes.PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_JOB.name().equals(jobProcessType)){
      if (fetchHourThreshold != Constants.ZERO.intValue()) {
        template =
            velocityEngine.getTemplate(Constants.PRODUCT_ATTRIBUTE_EXTRACTIONS_DELTA_TEMPLATE);
      } else {
        template =
            velocityEngine.getTemplate(Constants.PRODUCT_ATTRIBUTE_EXTRACTIONS_BY_ATTRIBUTE_NAME);
      }
    } else {
      template = velocityEngine.getTemplate(Constants.FULL_FETCH_SELLER_ANALYTICS_TEMPLATE);
    }
    return getFullQueryString(template, recordFetchSize, fetchHourThreshold, suggestedDateFetch,
        attributeName, autoApprovalFetchHour);
  }

  private String getFullQueryString(Template template, int recordFetchSize,int fetchHourThreshold
    , int suggestedDateFetch, String attributeName, int autoApprovalFetchHour) {
    VelocityContext velocityContext =
      getFullQueryContext(recordFetchSize, fetchHourThreshold, suggestedDateFetch, attributeName,
        autoApprovalFetchHour);
    StringWriter stringWriter = new StringWriter();
    template.merge(velocityContext, stringWriter);
    return stringWriter.toString();
  }

  private VelocityContext getFullQueryContext(int recordFetchSize, int fetchHourThreshold,
    int suggestedDateFetch, String attributeName, int autoApprovalFetchHour) {
    VelocityContext velocityContext = new VelocityContext();
    velocityContext.put(Constants.PROJECT_ID, gcpProperties.getQueryProjectId());
    velocityContext.put(Constants.RECORD_FETCH_SIZE, recordFetchSize);
    velocityContext.put(Constants.GENEVA_PROJECT_ID, gcpProperties.getGenevaProjectId());
    velocityContext.put(Constants.DATASCIENCE_PROJECT_ID, gcpProperties.getDatascienceProjectId());
    velocityContext.put(Constants.PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_TABLE,
        gcpProperties.getProductAttributeExtractionsBQTable());
    velocityContext.put(Constants.PRD_PRODUCT_BUSINESS_PARTNER_BQ_TABLE,
        gcpProperties.getPrdProductBusinessPartnerBQTable());
    velocityContext.put(Constants.FETCH_HOUR_THRESHOLD, fetchHourThreshold);
    velocityContext.put(Constants.PRODUCT_OPTIMISATION_DS_TABLE,
      gcpProperties.getProductOptimisationDsTable());
    velocityContext.put(Constants.SUGGESTED_DATE_FETCH, suggestedDateFetch);
    velocityContext.put(Constants.ATTRIBUTE_NAME, attributeName);
    velocityContext.put(Constants.IPR_PRODUCT_FETCH_TABLE, gcpProperties.getIprProductFetchTable());
    velocityContext.put(Constants.IPR_BIG_QUERY_PROJECT_ID, gcpProperties.getIprBigQueryProjectId());
    velocityContext.put(Constants.AUTO_APPROVAL_FETCH_HOUR, autoApprovalFetchHour);
    return velocityContext;
  }
}
