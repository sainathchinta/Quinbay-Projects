package com.gdn.partners.pcu.internal.service;

import java.util.List;

/**
 * Created by govind on 17/01/2019 AD.
 */
public interface CacheProductService {

  /**
   * API used to remove current user from list of logged in users reviewing a product
   * @param productCodeRedisKey
   * @param currentUserName
   * @return
   */
  String removeCurrentUserFromProductView(String productCodeRedisKey, String currentUserName);

  /**
   * return list of vendor who is reviewing the product concurrently
   * @param productCodeRedisKey
   * @return
   * @throws Exception
   */
  List<String> getReviewerList(String productCodeRedisKey);

  /**
   * add reviewer to redis.
   *
   * @param productCodeRedisKey
   * @param userName
   * @throws Exception
   */
  void addUserToProductReviewList(String productCodeRedisKey, String userName);
}
