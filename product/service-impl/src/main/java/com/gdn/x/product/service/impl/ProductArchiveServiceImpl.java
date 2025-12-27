package com.gdn.x.product.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ProductArchiveRepository;
import com.gdn.x.product.model.entity.ProductArchive;
import com.gdn.x.product.service.api.ProductArchiveService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ProductArchiveServiceImpl implements ProductArchiveService {

  @Autowired
  private ProductArchiveRepository productArchiveRepository;

  @Override
  public List<ProductArchive> addProductsToProductArchive(List<ProductArchive> productArchiveList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productArchiveList),
        ErrorMessages.PRODUCT_ARCHIVE_LIST_MUST_NOT_BE_EMPTY);
    return productArchiveRepository.saveAll(productArchiveList);
  }
}
