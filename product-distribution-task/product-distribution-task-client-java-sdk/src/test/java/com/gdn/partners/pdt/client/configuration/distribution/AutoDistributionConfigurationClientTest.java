package com.gdn.partners.pdt.client.configuration.distribution;

import java.util.ArrayList;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.verification.VerificationMode;
import org.springframework.test.util.ReflectionTestUtils;

import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pdt.dto.configuration.distribution.AutoDistributionConfigurationResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.CreateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.UpdateAutoDistributionConfigurationRequest;

public class AutoDistributionConfigurationClientTest {

  private static final String DEFAULT_HOST = "LOCALHOST";
  private static final Integer DEFAULT_PORT = 80;
  private static final String DEFAULT_CONTEXT_PATH = "PDT";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_VENDOR_CODE = "V-00001";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  private GdnHttpClientHelper gdnHttpClientHelper;
  private AutoDistributionConfigurationClient autoDistributionConfigurationClient;

  @BeforeEach
  public void initializeTest() throws Exception {
    GdnRestClientConfiguration clientConfig =
        new GdnRestClientConfiguration(AutoDistributionConfigurationClientTest.DEFAULT_USERNAME, null,
            AutoDistributionConfigurationClientTest.DEFAULT_HOST, AutoDistributionConfigurationClientTest.DEFAULT_PORT,
            AutoDistributionConfigurationClientTest.DEFAULT_CLIENT_ID,
            AutoDistributionConfigurationClientTest.DEFAULT_CHANNEL_ID,
            AutoDistributionConfigurationClientTest.DEFAULT_STORE_ID);
    this.autoDistributionConfigurationClient =
        new AutoDistributionConfigurationClient(clientConfig,
            AutoDistributionConfigurationClientTest.DEFAULT_CONTEXT_PATH);
    this.gdnHttpClientHelper = Mockito.mock(GdnHttpClientHelper.class);
    ReflectionTestUtils
        .setField(this.autoDistributionConfigurationClient, "httpClientHelper", this.gdnHttpClientHelper);
  }

  @Test
   void filterByVendorCodeTest() throws Exception {
    Mockito.when(
        this.gdnHttpClientHelper.invokeGetSummary(Mockito.any(), Mockito.any(),
            Mockito.anyString(), Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1),
            AutoDistributionConfigurationClientTest.DEFAULT_REQUEST_ID));
    GdnRestListResponse<AutoDistributionConfigurationResponse> response =
        this.autoDistributionConfigurationClient.filterByVendorCode(
            AutoDistributionConfigurationClientTest.DEFAULT_REQUEST_ID,
            AutoDistributionConfigurationClientTest.DEFAULT_USERNAME,
            AutoDistributionConfigurationClientTest.DEFAULT_VENDOR_CODE);
    Mockito.verify(this.gdnHttpClientHelper).invokeGetSummary(Mockito.any(), Mockito.any(),
        Mockito.anyString(), Mockito.any(), Mockito.any());
    Assertions.assertNotNull(response);
  }

  @Test
   void filterByVendorCodeWithApplicationRuntimeExceptionTest() throws Exception {
      Assertions.assertThrows(Exception.class,
        () -> this.autoDistributionConfigurationClient.filterByVendorCode(
          AutoDistributionConfigurationClientTest.DEFAULT_REQUEST_ID,
          AutoDistributionConfigurationClientTest.DEFAULT_USERNAME, null));
      Mockito.verify(this.gdnHttpClientHelper, AutoDistributionConfigurationClientTest.NEVER_CALLED)
        .invokeGetSummary(Mockito.any(HttpRequestBase.class), Mockito.any(), Mockito.anyString(),
          Mockito.any(CloseableHttpClient.class), Mockito.anyMap());
  }

  @Test
   void createTest() throws Exception {
    Mockito.when(
        this.gdnHttpClientHelper.invokePost(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(AutoDistributionConfigurationClientTest.DEFAULT_REQUEST_ID));
    CreateAutoDistributionConfigurationRequest request = new CreateAutoDistributionConfigurationRequest();
    GdnBaseRestResponse response =
        this.autoDistributionConfigurationClient.create(null, AutoDistributionConfigurationClientTest.DEFAULT_USERNAME,
            request);
    Mockito.verify(this.gdnHttpClientHelper).invokePost(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.anyString(), Mockito.any(),
        Mockito.any());
    Assertions.assertNotNull(response);
  }

  @Test
   void createWithApplicationRuntimeExceptionTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.autoDistributionConfigurationClient.create(null,
            AutoDistributionConfigurationClientTest.DEFAULT_USERNAME, null));
    Mockito.verify(this.gdnHttpClientHelper, AutoDistributionConfigurationClientTest.NEVER_CALLED)
        .invokePost(Mockito.any(HttpEntityEnclosingRequestBase.class), Mockito.any(), Mockito.any(),
            Mockito.anyString(), Mockito.any(CloseableHttpClient.class), Mockito.anyMap());
  }

  @Test
   void updateTest() throws Exception {
    Mockito.when(
        this.gdnHttpClientHelper.invokePost(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(AutoDistributionConfigurationClientTest.DEFAULT_REQUEST_ID));
    UpdateAutoDistributionConfigurationRequest request = new UpdateAutoDistributionConfigurationRequest();
    GdnBaseRestResponse response =
        this.autoDistributionConfigurationClient.update(null, AutoDistributionConfigurationClientTest.DEFAULT_USERNAME,
            request);
    Mockito.verify(this.gdnHttpClientHelper).invokePost(Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.anyString(), Mockito.any(),
        Mockito.any());
    Assertions.assertNotNull(response);
  }

  @Test
   void updateWithApplicationRuntimeExceptionTest() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> this.autoDistributionConfigurationClient.update(null, AutoDistributionConfigurationClientTest.DEFAULT_USERNAME,
          null));
        Mockito.verify(this.gdnHttpClientHelper, AutoDistributionConfigurationClientTest.NEVER_CALLED).invokePost(
            Mockito.any(HttpEntityEnclosingRequestBase.class), Mockito.any(), Mockito.any(),
            Mockito.anyString(), Mockito.any(CloseableHttpClient.class), Mockito.anyMap());
  }

}
