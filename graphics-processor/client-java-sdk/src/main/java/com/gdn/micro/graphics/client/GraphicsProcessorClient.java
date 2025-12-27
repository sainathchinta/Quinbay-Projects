package com.gdn.micro.graphics.client;

import java.net.URI;
import java.security.Security;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.RandomUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.ByteArrayBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestClient;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.micro.graphics.web.helper.ApiPath;
import com.gdn.micro.graphics.web.helper.ConvertImageResponse;
import com.gdn.micro.graphics.web.helper.ValidationKeyEncryptor;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.IdentifyImageResponse;
import com.gdn.micro.graphics.web.model.RemoveImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

public class GraphicsProcessorClient extends GdnBaseRestClient {

  private static final Logger LOG = LoggerFactory.getLogger(GraphicsProcessorClient.class);
  private static final String APPLICATION_JSON = "application/json";
  private ObjectMapper objectMapper;

  public GraphicsProcessorClient(GdnRestClientConfiguration clientConfig, String contextPath) {
    super(clientConfig);
    setContextPath(contextPath);
    Security.addProvider(new BouncyCastleProvider());
  }

  public GdnRestListResponse<ConvertImageResponse> convertImage(String requestId, String username,
      String imageName, byte[] content, List<CustomGraphicsSettings> settings,
      String[] targetTypeList) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put("settings", objectMapper.writeValueAsString(settings));
    additionalParameterMap.put("imageName", imageName);
    additionalParameterMap.put("targetTypeList",
        StringUtils.join(targetTypeList, ",").toUpperCase());

    HttpPost httpPost = generateMultipartHttpPost(ApiPath.CONVERT_OPERATION_PATH, content,
        requestId, imageName, username, additionalParameterMap);
    CloseableHttpResponse response = getHttpClient().execute(httpPost);
    if (response.getStatusLine().getStatusCode() == 200) {
      return objectMapper.readValue(EntityUtils.toString(response.getEntity()),
          new TypeReference<GdnRestListResponse<ConvertImageResponse>>() {});
    } else {
      String responseText = null;
      if (response.getEntity() != null) {
        responseText = EntityUtils.toString(response.getEntity());
      }
      LOG.error("server give bad response code, code : {}, message : {}, body: {}",
          new Object[] {response.getStatusLine().getStatusCode(),
              response.getStatusLine().getReasonPhrase(), responseText});
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "check the log");
    }
  }

  @SuppressWarnings("deprecation")
  private HttpPost generateMultipartHttpPost(String path, byte[] content, String requestId,
      String imageName, String username, Map<String, String> additionalParameterMap)
          throws Exception {
    HttpPost httpPost = getHttpClientHelper().createNewHttpPost(
        generateURI(path, requestId, additionalParameterMap, username),
        getClientConfig().getConnectionTimeoutInMs());
    MultipartEntityBuilder entityBuilder = MultipartEntityBuilder.create();
    entityBuilder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE).addPart("content",
        new ByteArrayBody(content, imageName));
    httpPost.setEntity(entityBuilder.build());
    httpPost.setHeader("Accept", APPLICATION_JSON);
    return httpPost;
  }

  @SuppressWarnings("deprecation")
  private URI generateURI(String path, String requestId, Map<String, String> additionalParameterMap,
      String username) throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(),
        this.getClientConfig().getPort(), location,
        this.getMandatoryParameter(getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  private CloseableHttpClient getHttpClient() {
    return getHttpClientHelper().getClosableHttpConnectionSingleton();
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public GdnRestSingleResponse<IdentifyImageResponse> identifyImage(String requestId,
      String username, String imageName, byte[] content) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    HttpPost httpPost = generateMultipartHttpPost(ApiPath.IDENTIFY_OPERATION_PATH, content,
        requestId, imageName, username, additionalParameterMap);
    CloseableHttpResponse response = getHttpClient().execute(httpPost);
    if (response.getStatusLine().getStatusCode() == 200) {
      return objectMapper.readValue(EntityUtils.toString(response.getEntity()),
          new TypeReference<GdnRestSingleResponse<IdentifyImageResponse>>() {});
    } else {
      String responseText = null;
      if (response.getEntity() != null) {
        responseText = EntityUtils.toString(response.getEntity());
      }
      LOG.error("server give bad response code, code : {}, message : {}, body: {}",
          new Object[] {response.getStatusLine().getStatusCode(),
              response.getStatusLine().getReasonPhrase(), responseText});
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "check the log");
    }
  }

  @SuppressWarnings("deprecation")
  public GdnBaseRestResponse removeImage(String requestId, String username, String imagePath)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    RemoveImageRequest dto = new RemoveImageRequest();
    String randomSeed = String.valueOf(RandomUtils.nextLong(1000000000000000L, 9999999999999999L));
    dto.setRandomSeed(randomSeed);
    ValidationKeyEncryptor keyEncryptor = new ValidationKeyEncryptor(randomSeed);
    dto.setValue(keyEncryptor.encrypt(getClientConfig().getClientId() + "," + imagePath));
    return invokePostType(
        generateURI(ApiPath.REMOVE_PATH, requestId, additionalParameterMap, username), dto,
        RemoveImageRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestListResponse<ConvertImageResponse> scaleImage(String requestId, String username, String groupCode,
      String imageName, byte[] content, List<CustomGraphicsSettings> settings) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put("settings", objectMapper.writeValueAsString(settings));
    additionalParameterMap.put("groupCode", groupCode);
    additionalParameterMap.put("imageName", imageName);
    HttpPost httpPost = generateMultipartHttpPost(ApiPath.SCALE_OPERATION_PATH, content, requestId,
        imageName, username, additionalParameterMap);
    CloseableHttpResponse response = getHttpClient().execute(httpPost);
    if (response.getStatusLine().getStatusCode() == 200) {
      return objectMapper.readValue(EntityUtils.toString(response.getEntity()),
          new TypeReference<GdnRestListResponse<ConvertImageResponse>>() {});
    } else {
      String responseText = null;
      if (response.getEntity() != null) {
        responseText = EntityUtils.toString(response.getEntity());
      }
      LOG.error("server give bad response code, code : {}, message : {}, body: {}",
          new Object[] {response.getStatusLine().getStatusCode(),
              response.getStatusLine().getReasonPhrase(), responseText});
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "check the log");
    }
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public byte[] showImage(String requestId, String imagePath) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put("imagePath", imagePath);
    @SuppressWarnings("deprecation")
    HttpGet httpGet = getHttpClientHelper().createNewHttpGet(
        generateURI(ApiPath.DISPLAY_PATH, requestId, additionalParameterMap, ""),
        getClientConfig().getConnectionTimeoutInMs());
    CloseableHttpResponse response = getHttpClient().execute(httpGet);
    if (response.getStatusLine().getStatusCode() == 200) {
      return EntityUtils.toByteArray(response.getEntity());
    } else {
      String responseText = null;
      if (response.getEntity() != null) {
        responseText = EntityUtils.toString(response.getEntity());
      }
      LOG.error("server give bad response code, code : {}, message : {}, body: {}",
          new Object[] {response.getStatusLine().getStatusCode(),
              response.getStatusLine().getReasonPhrase(), responseText});
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "check the log");
    }
  }

  public GdnRestSingleResponse<ConvertImageResponse> storeImage(String requestId, String username,
      String imageName, byte[] content) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<>();
    additionalParameterMap.put("imageName", imageName);
    HttpPost httpPost = generateMultipartHttpPost(ApiPath.STORE_OPERATION_PATH, content, requestId,
        imageName, username, additionalParameterMap);
    CloseableHttpResponse response = getHttpClient().execute(httpPost);
    if (response.getStatusLine().getStatusCode() == 200) {
      return objectMapper.readValue(EntityUtils.toString(response.getEntity()),
          new TypeReference<GdnRestSingleResponse<ConvertImageResponse>>() {});
    } else {
      String responseText = null;
      if (response.getEntity() != null) {
        responseText = EntityUtils.toString(response.getEntity());
      }
      LOG.error("server give bad response code, code : {}, message : {}, body: {}",
          new Object[] {response.getStatusLine().getStatusCode(),
              response.getStatusLine().getReasonPhrase(), responseText});
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "check the log");
    }
  }

  public GdnBaseRestResponse scaleBulkImages(String requestId, String username,
      BulkImagesProcessRequest request) throws Exception {
    return invokePostType(
        generateURI(ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH, requestId, null, username), request,
        BulkImagesProcessRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {},
        null);
  }

  public GdnBaseRestResponse resizeBulkImages(String requestId, String username, BulkResizeImageRequest request)
      throws Exception {
    return invokePostType(generateURI(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH, requestId, null, username), request,
        BulkResizeImageRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {
        }, null);
  }

  public GdnBaseRestResponse resizeEditedImages(String requestId, String username, BulkResizeImageRequest request)
      throws Exception {
    return invokePostType(generateURI(ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH, requestId, null, username), request,
        BulkResizeImageRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {
        }, null);
  }

  public GdnBaseRestResponse resizeRevisedImages(String requestId, String username, BulkResizeImageRequest request)
      throws Exception {
    return invokePostType(generateURI(ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH, requestId, null, username), request,
        BulkResizeImageRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {
        }, null);
  }

  public GdnBaseRestResponse scaleEditedImages(String requestId, String username, ScaleEditedImageRequest request)
      throws Exception {
    return invokePostType(generateURI(ApiPath.SCALE_EDITED_IMAGES_OPERATION_PATH, requestId, null, username), request,
        ScaleEditedImageRequest.class, APPLICATION_JSON, new TypeReference<GdnBaseRestResponse>() {
        }, null);
  }
}
