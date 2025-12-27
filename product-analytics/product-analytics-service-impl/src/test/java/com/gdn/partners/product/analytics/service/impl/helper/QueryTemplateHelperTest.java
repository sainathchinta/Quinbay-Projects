package com.gdn.partners.product.analytics.service.impl.helper;


import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.StringReader;

import org.apache.velocity.Template;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeServices;
import org.apache.velocity.runtime.RuntimeSingleton;
import org.apache.velocity.runtime.parser.ParseException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import org.mockito.junit.jupiter.MockitoExtension;
import org.apache.commons.lang3.StringUtils;

@ExtendWith(MockitoExtension.class)
public class QueryTemplateHelperTest {

  private static final String PROJECT_ID = "projectId";
  private static final String FULL_FETCH = "fullFetch";
  private static final String FULL_FETCH_TEMPLATE = "templates/fullFetch.vm";
  private static final String FULL_FETCH_SELLER_SPECIFIC_TEMPLATE = "templates/sellerSpecificFullFetch.vm";
  private static final String FULL_FETCH_SELLER_ANALYTICS_TEMPLATE =
    "templates/sellerAnalyticsFullFetch.vm";
  private static final String AUTO_APPROVED_PRODUCTS_TEMPLATE = "templates/autoApprovedProducts.vm";
  private static final String IPR_PRODUCTS_TEMPLATE = "templates/iprProducts.vm";
  private static final String PRODUCT_OPTIMISATION_TEMPLATE = "templates/productOptimisation.vm";
  private static final String PRODUCT_ATTRIBUTE_EXTRACTIONS_DELTA_TEMPLATE = "templates/productAttributeExtractionsDelta.vm";
  private static final String PRODUCT_ATTRIBUTE_EXTRACTIONS_BY_ATTRIBUTE_NAME = "templates/productAttributeExtractionsByAttributeName.vm";

  @InjectMocks
  private QueryTemplateHelper queryTemplateHelper;

  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private VelocityEngine velocityEngine;

  @BeforeEach
  public void setUp() {
    when(gcpProperties.getQueryProjectId()).thenReturn(PROJECT_ID);
  }

  @AfterEach
  public void tearDown() {
  }

  @Test
  public void getFullFetchQuery() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response = queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_INFO_BQ_JOB.name(),
      0, 24, 0, StringUtils.EMPTY, 0);
    verify(velocityEngine).getTemplate(FULL_FETCH_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  public void getFullFetchQuerySellerSpecific() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response = queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name(),
      0, 24, 0, StringUtils.EMPTY, 0);
    verify(velocityEngine).getTemplate(FULL_FETCH_SELLER_SPECIFIC_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  public void getFullFetchQuerySellerAnalyticsTest() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response =
      queryTemplateHelper.getFullFetchQuery(JobProcessTypes.SELLER_ANALYTICS_INFO_BQ_JOB.name(), 0, 24,
        0, StringUtils.EMPTY, 0);
    verify(velocityEngine).getTemplate(FULL_FETCH_SELLER_ANALYTICS_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  public void getFullFetchAutoApprovedQuery() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response =
      queryTemplateHelper.getFullFetchQuery(JobProcessTypes.AUTO_APPROVED_PRODUCTS_JOB.name(), 0, 24,
        0, StringUtils.EMPTY, 0);
    verify(velocityEngine).getTemplate(AUTO_APPROVED_PRODUCTS_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  public void getFullFetchIPRProductsQuery() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response =
        queryTemplateHelper.getFullFetchQuery(JobProcessTypes.IPR_PRODUCTS_JOB.name(), 0, 24, 0, StringUtils.EMPTY,
          0);
    verify(velocityEngine).getTemplate(IPR_PRODUCTS_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  void getFullFetchProductOptimisationQuery() throws ParseException {
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);
    String response =
      queryTemplateHelper.getFullFetchQuery(JobProcessTypes.PRODUCT_OPTIMISATION_JOB.name(), 0, 24,
        0, StringUtils.EMPTY, 0);
    verify(velocityEngine).getTemplate(PRODUCT_OPTIMISATION_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  void getFullFetchProductAttributeExtractionsDeltaQuery() throws ParseException {
    // Given
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);

    // When
    String response = queryTemplateHelper.getFullFetchQuery(
        JobProcessTypes.PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_JOB.name(), 0, 24, 0, StringUtils.EMPTY, 0);

    // Then
    verify(velocityEngine).getTemplate(PRODUCT_ATTRIBUTE_EXTRACTIONS_DELTA_TEMPLATE);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }

  @Test
  void getFullFetchProductAttributeExtractionsByAttributeName() throws ParseException {
    // Given
    String data = "${projectId}";
    RuntimeServices runtimeServices = RuntimeSingleton.getRuntimeServices();
    StringReader reader = new StringReader(data);
    Template template = new Template();
    template.setRuntimeServices(runtimeServices);
    template.setData(runtimeServices.parse(reader, FULL_FETCH));
    template.initDocument();
    when(velocityEngine.getTemplate(anyString())).thenReturn(template);

    // When
    String response = queryTemplateHelper.getFullFetchQuery(
        JobProcessTypes.PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_JOB.name(), 0, 0, 0, "testAttribute", 0);

    // Then
    verify(velocityEngine).getTemplate(PRODUCT_ATTRIBUTE_EXTRACTIONS_BY_ATTRIBUTE_NAME);
    verify(gcpProperties).getQueryProjectId();
    assertNotNull(response);
  }
}
