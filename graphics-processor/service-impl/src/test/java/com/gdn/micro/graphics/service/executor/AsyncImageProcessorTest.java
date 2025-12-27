package com.gdn.micro.graphics.service.executor;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.service.GraphicsProcessorService;

/**
 * Created by Vishal on 21/06/18.
 */
public class AsyncImageProcessorTest {


  private static final String HASH_CODE = "hashCode";
  private static final String SOURCE_PATH = "sourcePath";
  private static final String DESTINATION_PATH = "destinationPath";
  private static final String PREFIX_PATH = "prefixPath";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_ID = "clientId";

  private AsyncImageProcessor processor;
  private CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();


  @Mock
  private GraphicsProcessorService service;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    processor =
        new AsyncImageProcessor(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, CLIENT_ID, false, service, null);
    processor.setCommonImage(true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(service);
  }

  @Test
  void call() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setSuccess(true);
    imageResultDetail.setImagePathLocation(DESTINATION_PATH);
    imageResultDetail.setRequestId(REQUEST_ID);
    Mockito.when(service.scale(SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings, PREFIX_PATH, false, null))
        .thenReturn(imageResultDetail);
    ImageResponse imageResponse = processor.call();
    Mockito.verify(service)
        .scale(SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings, PREFIX_PATH, false, null);
    Assertions.assertTrue(imageResponse.isCommonImage());
  }

}
