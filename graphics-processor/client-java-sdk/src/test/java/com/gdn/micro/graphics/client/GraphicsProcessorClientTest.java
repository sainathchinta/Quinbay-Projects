package com.gdn.micro.graphics.client;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnHttpClientHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.micro.graphics.web.helper.ConvertImageResponse;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.GraphicDimension;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;


public class GraphicsProcessorClientTest {

  private static final Logger LOG = LoggerFactory.getLogger(GraphicsProcessorClient.class);

  private static final String USERNAME = "username";
  private static final String PASSWORD = "password";
  private static final String HOST = "localhost";
  private static final Integer PORT = 8080;
  private static final String CLIENT_ID = "local";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";

  private static final String APPLICATION_JSON_VALUE = "application/json";
  private static final String CONTEXT_PATH = "graphics-processor";


  private GdnRestClientConfiguration configuration;

  @Mock
  private GdnHttpClientHelper gdnHttpClientHelper;

  private GraphicsProcessorClient client;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    configuration = new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, PORT, CLIENT_ID, CHANNEL_ID, STORE_ID);
    client = new GraphicsProcessorClient(configuration, CONTEXT_PATH);
    ReflectionTestUtils.setField(client, "httpClientHelper", gdnHttpClientHelper, GdnHttpClientHelper.class);
  }

  @Test
  @Disabled
  public void testIdentifyCorrectImage() throws Exception {
    LOG.info(client.identifyImage(null, USERNAME, "sample1.jpg",
        IOUtils.toByteArray(this.getClass().getResourceAsStream("/sample1.jpg"))).toString());
  }

  @Test
  @Disabled
  public void testScaleImage() throws Exception {
    List<CustomGraphicsSettings> settings = new ArrayList<CustomGraphicsSettings>();
    settings.add(new CustomGraphicsSettings(70, 80, new GraphicDimension(300, 300)));
    GdnRestListResponse<ConvertImageResponse> response = client.scaleImage(null, USERNAME, null, "simon-console.jpg",
        IOUtils.toByteArray(this.getClass().getResourceAsStream("/simon-console.jpg")), settings);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(client.showImage(null, response.getContent().get(0).getGeneratedImageLocation()).length > 0);
    Assertions.assertTrue(
        client.removeImage(null, USERNAME, response.getContent().get(0).getGeneratedImageLocation()).isSuccess());
  }

  @Test
  void graphicsProcessorClientConstructorTest() {
    GraphicsProcessorClient graphicsProcessorClient = new GraphicsProcessorClient(
        new GdnRestClientConfiguration(USERNAME, PASSWORD, HOST, PORT, CLIENT_ID, CHANNEL_ID, STORE_ID), CONTEXT_PATH);
    Assertions.assertEquals(GraphicsProcessorClient.class, graphicsProcessorClient.getClass());
  }

  @Test
  void scaleBulkImagesTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(REQUEST_ID);
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    Mockito.when(this.gdnHttpClientHelper
        .invokePost((HttpEntityEnclosingRequestBase) Mockito.any(), Mockito.any(),
            Mockito.eq(BulkImagesProcessRequest.class), Mockito.eq(APPLICATION_JSON_VALUE),
            Mockito.any(CloseableHttpClient.class), Mockito.anyMap())).thenReturn(response);
    client.scaleBulkImages(REQUEST_ID, USERNAME, request);
  }

  @Test
  void resizeBulkImagesTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(REQUEST_ID);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    Mockito.when(this.gdnHttpClientHelper
        .invokePost((HttpEntityEnclosingRequestBase) Mockito.any(), Mockito.any(),
            Mockito.eq(BulkImagesProcessRequest.class), Mockito.eq(APPLICATION_JSON_VALUE),
            Mockito.any(CloseableHttpClient.class), Mockito.anyMap())).thenReturn(response);
    client.resizeBulkImages(REQUEST_ID, USERNAME, request);
  }

  @Test
  void resizeEditedImagesTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(REQUEST_ID);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    Mockito.when(this.gdnHttpClientHelper
        .invokePost((HttpEntityEnclosingRequestBase) Mockito.any(), Mockito.any(),
            Mockito.eq(BulkImagesProcessRequest.class), Mockito.eq(APPLICATION_JSON_VALUE),
            Mockito.any(CloseableHttpClient.class), Mockito.anyMap())).thenReturn(response);
    client.resizeEditedImages(REQUEST_ID, USERNAME, request);
  }

  @Test
  void resizeRevisedImagesTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(REQUEST_ID);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    Mockito.when(this.gdnHttpClientHelper
        .invokePost((HttpEntityEnclosingRequestBase) Mockito.any(), Mockito.any(),
            Mockito.eq(BulkImagesProcessRequest.class), Mockito.eq(APPLICATION_JSON_VALUE),
            Mockito.any(CloseableHttpClient.class), Mockito.anyMap())).thenReturn(response);
    client.resizeRevisedImages(REQUEST_ID, USERNAME, request);
  }

  @Test
  void scaleEditedImagesTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(REQUEST_ID);
    ScaleEditedImageRequest request = new ScaleEditedImageRequest();
    Mockito.when(this.gdnHttpClientHelper
        .invokePost((HttpEntityEnclosingRequestBase) Mockito.any(), Mockito.any(),
            Mockito.eq(BulkImagesProcessRequest.class), Mockito.eq(APPLICATION_JSON_VALUE),
            Mockito.any(CloseableHttpClient.class), Mockito.anyMap())).thenReturn(response);
    client.scaleEditedImages(REQUEST_ID, USERNAME, request);
  }
}
