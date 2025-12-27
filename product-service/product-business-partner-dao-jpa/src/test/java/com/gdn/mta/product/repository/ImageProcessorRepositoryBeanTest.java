package com.gdn.mta.product.repository;

import java.util.UUID;

import com.gdn.partners.pbp.outbound.xgp.feign.XgpFeign;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

@SuppressWarnings("unchecked")
public class ImageProcessorRepositoryBeanTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";

  
  @Mock
  private XgpFeign xgpFeign;

  @InjectMocks
  private ImageProcessorRepositoryBean imageProcessorRepositoryBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(imageProcessorRepositoryBean, "graphicProcessorClientId", "seoul");
    ReflectionTestUtils.setField(imageProcessorRepositoryBean, "graphicProcessorResizeClientId", "resize");
    ReflectionTestUtils.setField(imageProcessorRepositoryBean, "editedProductsProcessorClientId", "scale");
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, StringUtils.EMPTY);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, StringUtils.EMPTY);
    Mockito.verifyNoMoreInteractions(xgpFeign);
  }

  @Test
  public void scaleImageBulkTest() throws Exception {
    BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.scaleBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkImagesProcessRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.imageProcessorRepositoryBean.scaleImage(bulkImagesProcessRequest);
    Mockito.verify(this.xgpFeign)
        .scaleBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(BulkImagesProcessRequest.class));
  }

  @Test
  public void scaleImageBulkFailTest() throws Exception {
    BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.scaleBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkImagesProcessRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try{
      this.imageProcessorRepositoryBean.scaleImage(bulkImagesProcessRequest);
    } catch (ApplicationException ex) {
      Mockito.verify(this.xgpFeign)
          .scaleBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
              Mockito.any(BulkImagesProcessRequest.class));
    }
  }

  @Test
  public void resizeImageBulkTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.imageProcessorRepositoryBean.resizeImage(bulkResizeImageRequest);
    Mockito.verify(this.xgpFeign)
        .resizeBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(BulkResizeImageRequest.class));
  }

  @Test
  public void resizeImageBulkFailTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try{
      this.imageProcessorRepositoryBean.resizeImage(bulkResizeImageRequest);
    } catch (ApplicationException ex) {
      Mockito.verify(this.xgpFeign)
          .resizeBulkImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
              Mockito.any(BulkResizeImageRequest.class));
    }
  }

  @Test
  public void resizeEditedImageTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.imageProcessorRepositoryBean.resizeEditedImage(bulkResizeImageRequest);
    Mockito.verify(this.xgpFeign)
        .resizeEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(BulkResizeImageRequest.class));
  }

  @Test
  public void resizeEditedImageFailTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try{
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.imageProcessorRepositoryBean.resizeEditedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(this.xgpFeign)
          .resizeEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
              Mockito.any(BulkResizeImageRequest.class));
    }
  }


  @Test
  public void xgpFeign() throws Exception {
    ScaleEditedImageRequest scaleEditedImageRequest = new ScaleEditedImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.scaleEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(ScaleEditedImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.imageProcessorRepositoryBean.scaleEditedImages(scaleEditedImageRequest);
    Mockito.verify(this.xgpFeign)
        .scaleEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(ScaleEditedImageRequest.class));
  }

  @Test
  public void xgpFeignFailTest() throws Exception {
    ScaleEditedImageRequest scaleEditedImageRequest = new ScaleEditedImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.scaleEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(ScaleEditedImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try{
      this.imageProcessorRepositoryBean.scaleEditedImages(scaleEditedImageRequest);
    } catch (ApplicationException ex) {
      Mockito.verify(this.xgpFeign)
          .scaleEditedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
              Mockito.any(ScaleEditedImageRequest.class));
    }
  }

  @Test
  public void resizeRevisedImageTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeRevisedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.imageProcessorRepositoryBean.resizeRevisedImage(bulkResizeImageRequest);
    Mockito.verify(this.xgpFeign)
        .resizeRevisedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(BulkResizeImageRequest.class));
  }

  @Test
  public void resizeRevisedImageExceptionTest() throws Exception {
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        DEFAULT_REQUEST_ID);
    Mockito.when(xgpFeign.resizeRevisedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.any(BulkResizeImageRequest.class))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try{
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.imageProcessorRepositoryBean.resizeRevisedImage(bulkResizeImageRequest);
      });
    } finally {
      Mockito.verify(this.xgpFeign)
          .resizeRevisedImages(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
              Mockito.any(BulkResizeImageRequest.class));
    }
  }
}
