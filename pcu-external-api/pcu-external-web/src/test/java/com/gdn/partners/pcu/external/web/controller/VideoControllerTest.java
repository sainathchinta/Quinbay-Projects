package com.gdn.partners.pcu.external.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.VideoApiPath;
import com.gdn.partners.pcu.external.service.VideoService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import jakarta.servlet.ServletException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.util.NestedServletException;


import java.util.Map;

import static com.gdn.partners.pcu.external.model.Constants.BSC;
import static com.gdn.partners.pcu.external.model.Constants.CLIENT_ID;
import static com.gdn.partners.pcu.external.model.Constants.REELS;
import static com.gdn.partners.pcu.external.model.Constants.VIDEO;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class VideoControllerTest extends TestHelper {

  private ObjectMapper objectMapper;

  @Mock
  private VideoService videoService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private VideoController videoController;

  private VideoSignedUrlRequest request;
  private VideoSignedUrlResponse response;
  private static final String REQUEST_ID = "test-request-id";

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    objectMapper = new ObjectMapper();
    mockMvc = MockMvcBuilders.standaloneSetup(videoController).build();

    request = new VideoSignedUrlRequest();
    request.setClientId("test-client");
    request.setSource("test-owner-type");
    request.setOwnerCode("test-owner-code");
    request.setVideoName("test-video.mp4");
    request.setCompression(true);

    response = new VideoSignedUrlResponse();
    response.setVideoSignedUrl("https://test-signed-url.com/video");
    response.setVideoId("test-video-id");
    response.setVideoUploadUrl("https://test-upload-url.com/video");
    response.setImageSignedUrl("https://test-signed-url.com/image");
    response.setImageUploadUrl("https://test-upload-url.com/image");
    response.setExpiresAt(System.currentTimeMillis() + 3600000); // 1 hour from now

    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
  }

  @Test
  void generateSignedUrl_Success() throws Exception {
    request.setClientId(BSC);
    request.setSource(VIDEO);
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(
        true);
    when(videoService.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(response);
    mockMvc.perform(
            post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.value.videoSignedUrl").value(response.getVideoSignedUrl()))
        .andExpect(jsonPath("$.value.videoId").value(response.getVideoId()))
        .andExpect(jsonPath("$.value.videoUploadUrl").value(response.getVideoUploadUrl()))
        .andExpect(jsonPath("$.value.imageSignedUrl").value(response.getImageSignedUrl()))
        .andExpect(jsonPath("$.value.imageUploadUrl").value(response.getImageUploadUrl()))
        .andExpect(jsonPath("$.value.expiresAt").value((int) response.getExpiresAt()));
  }
  @Test
  void generateSignedUrl_Success_with_videoNameContains() throws Exception {
    request.setClientId(BSC);
    request.setSource(VIDEO);
    request.setVideoName("video/samplevideo.mp4");
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(
        true);
    when(videoService.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(response);
    mockMvc.perform(
            post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.value.videoSignedUrl").value(response.getVideoSignedUrl()))
        .andExpect(jsonPath("$.value.videoId").value(response.getVideoId()))
        .andExpect(jsonPath("$.value.videoUploadUrl").value(response.getVideoUploadUrl()))
        .andExpect(jsonPath("$.value.imageSignedUrl").value(response.getImageSignedUrl()))
        .andExpect(jsonPath("$.value.imageUploadUrl").value(response.getImageUploadUrl()))
        .andExpect(jsonPath("$.value.expiresAt").value((int) response.getExpiresAt()));
  }
  @Test
  void generateSignedUrl_Success_with_videoNameNull() throws Exception {
    request.setClientId(BSC);
    request.setSource(VIDEO);
    request.setVideoName(null);
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(
        true);
    when(videoService.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(response);
    mockMvc.perform(
            post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.value.videoSignedUrl").value(response.getVideoSignedUrl()))
        .andExpect(jsonPath("$.value.videoId").value(response.getVideoId()))
        .andExpect(jsonPath("$.value.videoUploadUrl").value(response.getVideoUploadUrl()))
        .andExpect(jsonPath("$.value.imageSignedUrl").value(response.getImageSignedUrl()))
        .andExpect(jsonPath("$.value.imageUploadUrl").value(response.getImageUploadUrl()))
        .andExpect(jsonPath("$.value.expiresAt").value((int) response.getExpiresAt()));
  }

  @Test
  void generateSignedUrl_ThrowValidationException_WhenVideoFeatureDisabled() throws Exception {
    request.setClientId(BSC);
    request.setSource(VIDEO);

    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(false);

    ServletException exception = assertThrows(ServletException.class, () -> {
      mockMvc.perform(post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL)
          .contentType(MediaType.APPLICATION_JSON)
          .accept(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(request)));
    });

    // Assert the actual cause of the failure
    Throwable cause = exception.getCause();
    Assertions.assertTrue(cause instanceof ValidationException);
    assertEquals("AUTHORIZATION", cause.getMessage());
  }

  @Test
  void generateSignedUrl_ShouldNotThrow_WhenClientIdIsNotBSC() throws Exception {
    request.setClientId(CLIENT_ID); // Not BSC
    request.setSource(VIDEO);
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(false);

    when(videoService.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(response);

    mockMvc.perform(post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
  }

  @Test
  void generateSignedUrl_ShouldNotThrow_WhenSourceIsNotVideo() throws Exception {
    request.setClientId(BSC);
    request.setSource(REELS); // Not VIDEO
    when(mandatoryParameterHelper.getIsProductVideoActivated()).thenReturn(false);

    when(videoService.generateSignedUrl(any(VideoSignedUrlRequest.class))).thenReturn(response);

    mockMvc.perform(post(VideoApiPath.BASE_PATH + VideoApiPath.GENERATE_SIGNED_URL)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk());
  }

  @Test
  void getVideoConfiguration_Success() throws Exception {
    Map<String, Object> configMap =
        Map.of("minimumDuration", 10, "maximumDuration", 90, "maximumFileSize", 30, "format",
            "video/mp4", "height", 1280, "width", 1280);
    when(mandatoryParameterHelper.getRequestId()).thenReturn("test-request-id");
    when(videoService.fetchVideoConfiguration(any(VideoConfigurationRequest.class))).thenReturn(
        configMap);
    mockMvc.perform(post(VideoApiPath.BASE_PATH + VideoApiPath.FETCH_CONFIGURATION).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(new VideoConfigurationRequest())))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true))
        .andExpect(jsonPath("$.requestId").value("test-request-id"))
        .andExpect(jsonPath("$.value.minimumDuration").value(10))
        .andExpect(jsonPath("$.value.maximumDuration").value(90));
  }

} 