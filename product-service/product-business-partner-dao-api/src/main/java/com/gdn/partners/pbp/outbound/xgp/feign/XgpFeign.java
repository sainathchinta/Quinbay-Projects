package com.gdn.partners.pbp.outbound.xgp.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;
import org.springframework.web.bind.annotation.RequestParam;

public interface XgpFeign {
  @RequestLine("POST /api/operation/scale-bulk-Images?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse scaleBulkImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      BulkImagesProcessRequest bulkImagesProcessRequest);


  @RequestLine("POST /api/operation/scale-edited-Images?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse scaleEditedImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      ScaleEditedImageRequest scaleEditedImageRequest);

  @RequestLine("POST /api/operation/resize-bulk-Images?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse resizeBulkImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      BulkResizeImageRequest bulkResizeImageRequest);

  @RequestLine("POST /api/operation/resize-edited-Images?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse resizeEditedImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      BulkResizeImageRequest bulkResizeImageRequest);

  @RequestLine("POST /api/operation/resize-revised-images?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse resizeRevisedImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      BulkResizeImageRequest bulkResizeImageRequest);


}
