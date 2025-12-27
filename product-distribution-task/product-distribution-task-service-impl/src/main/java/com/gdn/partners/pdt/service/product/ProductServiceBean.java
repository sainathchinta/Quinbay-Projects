package com.gdn.partners.pdt.service.product;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;

@Service("neoProductService")
@Transactional(readOnly = true)
public class ProductServiceBean implements ProductService {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductUtils productUtils;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void overwrite(Product savedProduct, Product product) throws Exception {
    this.clearProductDetails(savedProduct);
    this.productUtils.regenerateProductReplacementDetails(savedProduct, product);
    this.productRepository.saveAndFlush(savedProduct);
  }

  private void clearProductDetails(Product savedProduct) {
    if (!CollectionUtils.isEmpty(savedProduct.getProductItems())) {
      this.productItemRepository.deleteAll(savedProduct.getProductItems());
    }
    if (!CollectionUtils.isEmpty(savedProduct.getProductAttributes())) {
      this.productAttributeRepository.deleteAll(savedProduct.getProductAttributes());
    }
    if (!CollectionUtils.isEmpty(savedProduct.getProductImages())) {
      this.productImageRepository.deleteAll(savedProduct.getProductImages());
    }
    savedProduct.getProductItems().clear();
    savedProduct.getProductAttributes().clear();
    savedProduct.getProductImages().clear();
  }
}
