package com.gdn.mta.product.repository;

import com.gdn.partners.pbp.outbound.xgp.feign.XgpFeign;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

@Repository
public class ImageProcessorRepositoryBean implements ImageProcessorRepository {

  @Autowired
  private XgpFeign xgpFeign;

  @Value("${graphic.processor.client.id}")
  private String graphicProcessorClientId;

  @Value("${resize.image.client.id.default}")
  private String graphicProcessorResizeClientId;

  @Value("${edited.products.client.id.default}")
  private String editedProductsProcessorClientId;

  @Override
  public void scaleImage(BulkImagesProcessRequest bulkImagesProcessRequest) throws Exception {
    GdnBaseRestResponse response = this.xgpFeign.scaleBulkImages(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), graphicProcessorClientId,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        bulkImagesProcessRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void scaleEditedImages(ScaleEditedImageRequest scaleEditedImageRequest) throws Exception {
    GdnBaseRestResponse response = this.xgpFeign.scaleEditedImages(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), editedProductsProcessorClientId,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        scaleEditedImageRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void resizeImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception {
    GdnBaseRestResponse response = this.xgpFeign.resizeBulkImages(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), graphicProcessorResizeClientId,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        bulkResizeImageRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void resizeEditedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception {
    GdnBaseRestResponse response = this.xgpFeign.resizeEditedImages(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), graphicProcessorResizeClientId,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        bulkResizeImageRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void resizeRevisedImage(BulkResizeImageRequest bulkResizeImageRequest) throws Exception {
    GdnBaseRestResponse response = this.xgpFeign.resizeRevisedImages(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), graphicProcessorResizeClientId,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        bulkResizeImageRequest);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
  }
}
