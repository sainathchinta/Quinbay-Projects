package com.gdn.x.productcategorybase.service.notificationService;

import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface NotificationService {
  void createWebNotification(BrandWip brandWip) throws Exception;
}
