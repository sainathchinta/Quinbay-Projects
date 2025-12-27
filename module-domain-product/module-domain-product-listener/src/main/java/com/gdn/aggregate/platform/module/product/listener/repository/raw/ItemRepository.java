package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.List;
import java.util.stream.Stream;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.ItemRepositoryCustom;
import org.springframework.context.annotation.Primary;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
@Primary
public interface ItemRepository extends MongoRepository<Item, String>, ItemRepositoryCustom {

    @Query(fields = "{'_id' : 1}")
    Stream<Item> streamAllByProductSkuAndArchivedFalseAndMarkForDeleteFalse(String productSku);

    @Query(fields = "{'_id' : 1}")
    Item findFirstByProductSkuOrderByArchivedAscMarkForDeleteAsc(String productSku);

    Stream<Item> streamAllByItemCode(String itemCode);

    List<Item> findAllByProductSkuOrderByArchivedAscMarkForDeleteAsc(String productSku, Pageable pageable);

    boolean existsByProductSkuAndArchivedFalse(String productSku);

    boolean existsByProductSkuAndMarkForDeleteFalse(String productSku);

    List<Item> findAllByProductSku(String productSku);
    // Hard Delete Raw L4 and get count of deleted documents
    void deleteByProductSku(String productSku);
}
