package com.gdn.micro.graphics.service;

import java.util.List;

import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.ResizeImageScalingModel;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.XgpImageScaleRequest;

/**
 * Created by Vishal on 21/06/18.
 */
public interface AsyncGraphicsProcessService {

  /**
   * scale images in bulk
   *
   * @param graphicImageDetails must not null
   * @param groupCode must not blank
   * @param clientId must not blank
   * @param isRevised must not blank
   * @param prioritySeller
   * @throws Exception
   */
  void scaleBulkImages(final List<GraphicImageDetail> graphicImageDetails, final String groupCode, final String clientId, final boolean isRevised,
      int prioritySeller) throws Exception;

  /**
   * resize images in bulk
   *
   * @param graphicImageDetails
   * @param groupCode
   * @param clientId
   * @throws Exception
   */
  void resizeBulkImages(final List<GraphicImageDetail> graphicImageDetails,
      final String groupCode, final String clientId, int prioritySeller) throws Exception;

  /**
   * resize edited images which are newly added
   *
   * @param graphicImageDetails
   * @param groupCode
   * @param clientId
   * @throws Exception
   */
  void resizeEditedImages(final List<GraphicImageDetail> graphicImageDetails,
      final String groupCode, final String clientId) throws Exception;

  /**
   * resize edited images which are newly added
   *
   * @param graphicImageDetails
   * @param groupCode
   * @param clientId
   * @throws Exception
   */
  void resizeRevisedImages(final List<GraphicImageDetail> graphicImageDetails,
      final String groupCode, final String clientId) throws Exception;

  /**
   * scale newly added images
   *
   * @param customGraphicsSettings
   * @param groupCode
   * @param clientId
   * @param request
   * @throws Exception
   */
  void scaleEditedImages(final List<CustomGraphicsSettings> customGraphicsSettings, final String groupCode,
      final String clientId, ScaleEditedImageRequest request, String prefix) throws Exception;

  /**
   * process image scaling
   * @param graphicImageDetails
   * @param groupCode
   * @param username
   * @param storeId
   * @param clientId
   * @param isResize
   * @param isEdited
   * @param isRevised
   * @param request
   */
  void processImages(final List<GraphicImageDetail> graphicImageDetails, final String groupCode, final String username,
      final String storeId, final String clientId, boolean isResize, boolean isEdited, boolean isRevised,
      ScaleEditedImageRequest request, int prioritySeller);

  /**
   *
   * @param resizeImageScalingModel Scale Image Request
   */
  void scaleListOfImages(ResizeImageScalingModel resizeImageScalingModel) throws Exception;

    /**
     * Scale active product new images
     *
     * @param storeId              StoreId identifier
     * @param clientId             ClientId identifier
     * @param xgpImageScaleRequest XgpImageScaleRequest containing the details for scaling images
     * @param requestId
     */
  void scaleActiveProductNewImages(String storeId, String clientId, XgpImageScaleRequest xgpImageScaleRequest,
    String requestId)
    throws Exception;
}
