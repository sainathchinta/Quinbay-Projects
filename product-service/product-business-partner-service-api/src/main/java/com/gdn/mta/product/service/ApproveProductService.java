package com.gdn.mta.product.service;

import java.util.Map;

import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface ApproveProductService {

  /**
   * Request for approve QC
   *
   * @param productCode
   * @throws Exception
   */
  void approveQc(String productCode) throws Exception;

  /**
   * request update product content in PCB and approve content
   * @param request
   * @throws Exception
   */
  void approveContent(ProductRequest request) throws Exception;

  /**
   * request to update image content in PCB
   *
   * @param request
   * @throws Exception
   */
  void updateImage(ProductRequest request) throws Exception;


  /**
   * request to update images content in PCB and Avoid hashcode generation for post live not
   * edited images
   *
   * @param request product request
   * @throws Exception
   */
  void updateImageAndAvoidHashCodeRegeneration(ProductRequest request) throws Exception;

  /**
   * request update product image content in PCB and change workflow status to process image
   *
   * @param request
   * @throws Exception
   */
  void approveImage(ProductRequest request, int prioritySeller) throws Exception;

  /**
   * process eligible images for XGP Scaling after approval from vendor.
   *
   * @param request Product request
   * @param prioritySeller sellers priority
   * @param productImageHashCodeMapDB Map of Hash code and Product Image
   * @param itemImageHashCodeMapDB Map of Hash code and Item Image
   * @throws Exception
   */
  void processImageWithScalingEligibility(ProductRequest request, int prioritySeller,
    Map<String, Image> productImageHashCodeMapDB, Map<String, Image> itemImageHashCodeMapDB)
    throws Exception;

  /**
   * process images after image approval from vendor.
   * send images to XGP to scale them
   *
   * @param request
   * @param prioritySeller
   * @throws Exception
   */
  void processImage(ProductRequest request, int prioritySeller) throws Exception;

  /**
   * process images after image approval from vendor.
   * send images to XGP to scale them
   *
   * @param productDetailResponse
   * @throws Exception
   */
  void processImageForRevisedProduct(ProductDetailResponse productDetailResponse) throws Exception;

  /**
   * process images after image approval from vendor.
   * send images to XGP to scale them
   *
   * @param request
   * @throws Exception
   */
  void processEditedImage(ProductRequest request) throws Exception;

  /**
   * process images for skip review true products.
   * send images to XGP to scale them
   *
   * @param response
   * @throws Exception
   */
  void processImage(ProductDetailResponse response) throws Exception;

}
