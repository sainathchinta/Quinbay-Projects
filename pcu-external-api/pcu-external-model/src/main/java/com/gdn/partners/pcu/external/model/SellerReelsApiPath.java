package com.gdn.partners.pcu.external.model;

public interface SellerReelsApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/seller-reels";
  String LISTING = "/listing";
  String UPDATE = "/update";
  String ADD_REEL = "/add-reel";
  String DETAILS = "/details";
  String DELETE = "/{videoId}/delete";
}