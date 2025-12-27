package com.gdn.mta.product.service.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ApproveProductUtils {

  public static String generateHashcode(MessageDigest imageDigester) {
    byte[] digestBytes = imageDigester.digest();
    StringBuilder hashcode = new StringBuilder();
    for (int i = 0; i < digestBytes.length; i++) {
      hashcode.append(Integer.toString((digestBytes[i] & 0xff) + 0x100, 16).substring(1));
    }
    return hashcode.toString();
  }

  public static String generateHashcodeByLocationPath(String imageLocationPath) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("MD5");
      messageDigest.update(imageLocationPath.getBytes());
      byte[] digestBytes = messageDigest.digest();
      StringBuilder hashcode = new StringBuilder();
      for (int i = 0; i < digestBytes.length; i++) {
        hashcode.append(Integer.toString((digestBytes[i] & 0xff) + 0x100, 16).substring(1));
      }
      return hashcode.toString();
    } catch (NoSuchAlgorithmException e) {
      log.error("Invalid Algorithm used for hashing", e);
      return null;
    }
  }

  public static ProductRequest generateImageHashcode(ProductRequest request) throws Exception {
    MessageDigest imageDigester = MessageDigest.getInstance("MD5");
    for (Image productImage : request.getImages()) {
      imageDigester.update(productImage.getLocationPath().getBytes());
      productImage.setHashCode(generateHashcode(imageDigester));
    }
    for (ProductItemRequest productItemRequest : request.getProductItems()) {
      for (Image productItemImage : productItemRequest.getImages()) {
        imageDigester.update(productItemImage.getLocationPath().getBytes());
        productItemImage.setHashCode(generateHashcode(imageDigester));
      }
    }
    return request;
  }
}
