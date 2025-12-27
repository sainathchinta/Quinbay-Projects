package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.stream.Stream;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

public interface ProductRepository extends MongoRepository<Product, String> {

    @Query(value = "{'merchantCode' : ?0}", fields = "{'productSku' : 1}")
    Stream<Product> getProductSkusByMerchantCode(String merchantCode);

    Stream<Product> streamAllByProductCode(String productCode);

    Product findByProductSku(String productSku);
    // Hard Delete Raw L3 by productSku
    void deleteByProductSku(String productSku);
}
