package com.gdn.x.mta.distributiontask.client.util;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.DeserializationProblemHandler;
import com.gdn.common.client.GdnRestClientConfiguration;

import java.io.IOException;
import java.net.URI;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

/**
 * Created by virajjasani on 14/11/16.
 */
public class PDTClientUtil {

  public static <T extends Object> T invokeGetType(URI uri, TypeReference<T> typeReference,
      GdnRestClientConfiguration clientConfiguration) throws Exception {
    T result = null;
    CloseableHttpResponse response = null;
    HttpRequestBase request = new HttpGet(uri);
    CloseableHttpClient client = HttpClients.custom().setDefaultRequestConfig(
        RequestConfig.custom().setSocketTimeout(clientConfiguration.getConnectionTimeoutInMs())
            .setConnectTimeout(clientConfiguration.getConnectionTimeoutInMs()).build()).build();
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    objectMapper.addHandler(new DeserializationProblemHandler() {
      @Override
      public boolean handleUnknownProperty(DeserializationContext ctxt, JsonParser jp,
          JsonDeserializer<?> deserializer, Object beanOrClass, String propertyName) throws
          IOException,
          JsonProcessingException {
        return true;
      }
    });
    try {
      response = client.execute(request);
      if (response.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
        HttpEntity entity = response.getEntity();
        String responseBody = EntityUtils.toString(entity);
        int startIndex = responseBody.indexOf("{");
        int endIndex = responseBody.lastIndexOf("}") + 1;
        result =
            objectMapper.readValue(responseBody.substring(startIndex, endIndex), typeReference);
      } else {
        processUnexpectedResponse(response, result);
      }
    } catch (Exception e) {
      throw e;
    } finally {
      if (response != null) {
        response.close();
      }
    }
    return result;
  }

  public static <T extends Object> void processUnexpectedResponse(HttpResponse response, T result)
      throws Exception {
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    if (response.getStatusLine().getStatusCode() == HttpStatus.SC_INTERNAL_SERVER_ERROR) {
      result = objectMapper.readValue(response.getEntity().getContent(), new TypeReference<T>() {
      });
    } else {
      throw new Exception(
          "Http error code : " + response.getStatusLine().getStatusCode() + ", Reason : "
              + response.getStatusLine().getReasonPhrase());
    }
  }

}
