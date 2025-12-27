package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import java.util.Map;

public interface VideoService {
  /**
   * Generate Signed URL
   *
   * @param videoSignedUrlRequest
   * @return
   * @throws Exception
   */
  VideoSignedUrlResponse generateSignedUrl(VideoSignedUrlRequest videoSignedUrlRequest) throws Exception;

  /**
   * fetch video configuration
   *
   * @param videoConfigurationRequest
   * @return
   * @throws Exception
   */
  Map<String, Object> fetchVideoConfiguration(VideoConfigurationRequest videoConfigurationRequest) throws Exception;

    /**
     * Retry video compression
     *
     * @param videoId
     * @throws Exception
     */
  void retryVideoCompression(String videoId) throws Exception;

    /**
     * Generate fingerprint for a video
     *
     * @param videoId
     * @throws Exception
     */
  void generateFingerPrint(String videoId) throws Exception;
}
