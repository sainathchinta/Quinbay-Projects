package com.gdn.x.mta.distributiontask.domain.event.model;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;

public class PDTProductItemDomainEventModel extends ProductItemDomainEventModel {

  private List<ProductItemAttributeValueDomainEventModel> productItemAttributeValues;
  private List<PDTImageDomainEventModel> pdtImageDomainEventModels = new ArrayList<>();
  private PDTItemNotesDomainEventModel itemNotes;

  public List<ProductItemAttributeValueDomainEventModel> getProductItemAttributeValues() {
    return productItemAttributeValues;
  }

  public void setProductItemAttributeValues(
      List<ProductItemAttributeValueDomainEventModel> productItemAttributeValues) {
    this.productItemAttributeValues = productItemAttributeValues;
  }

  public List<PDTImageDomainEventModel> getPdtImageDomainEventModels() {
    return pdtImageDomainEventModels;
  }

  public void setPdtImageDomainEventModels(List<PDTImageDomainEventModel> pdtImageDomainEventModels) {
    this.pdtImageDomainEventModels = pdtImageDomainEventModels;
  }

  public PDTItemNotesDomainEventModel getItemNotes() {
    return itemNotes;
  }

  public void setItemNotes(PDTItemNotesDomainEventModel itemNotes) {
    this.itemNotes = itemNotes;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PDTProductItemDomainEventModel{");
    sb.append("ProductItemDomainEventModel=").append(super.toString());
    sb.append("productItemAttributeValues=").append(productItemAttributeValues);
    sb.append("pdtImageDomainEventModels=").append(pdtImageDomainEventModels);
    sb.append("itemNotes=").append(itemNotes);
    sb.append('}');
    return sb.toString();
  }
}
