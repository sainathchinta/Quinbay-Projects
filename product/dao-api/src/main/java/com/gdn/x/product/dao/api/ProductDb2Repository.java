package com.gdn.x.product.dao.api;

import java.util.List;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.TicketTemplate;
import com.gdn.x.product.model.vo.ProductAndItemsVO;


public interface ProductDb2Repository {

  List<TicketTemplate> findAllDb2TicketTemplate() throws Exception;

  ProductAndItemsVO findDb2Attributes(ProductAndItemsVO produtAndItemsVo) throws Exception;

  List<Item> findDb2ItemsData(Product product) throws Exception;

  Product findDb2ProductData(String productSku) throws Exception;

  ProductAndItemsVO findDb2ProductImages(ProductAndItemsVO productAndItemsVo) throws Exception;

}
