package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
class FieldServiceTest {

  @Mock
  FieldApi fieldApi;

  @InjectMocks
  FieldService fieldService;

  @Captor
  ArgumentCaptor<RequestPurpose> requestPurposeArgumentCaptor;

  List<Field> fields;

  @BeforeEach
  void setup() {
    fields = List.of(
        Field.newBuilder()
            .fieldId(1)
            .fieldName("test field 1")
            .build(),
        Field.newBuilder()
            .fieldId(2)
            .fieldName("test field 2")
            .build()
    );
  }

  @Test
  void getFieldsByName_assertReturn() {
    var searchTerm = "test";
    var purpose = "Test mock api request purpose";
    var statuses = Arrays.stream(FieldStatus.values())
        .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
        .toList();
    var argumentCaptor = ArgumentCaptor.forClass(FieldsProjectionRoot.class);
    var requestPurposeArgCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(fieldApi.searchFields(eq(searchTerm), eq(statuses), any(), any(RequestPurpose.class)))
        .thenReturn(fields);

    var queryResults = fieldService.getFieldsByName(searchTerm, purpose);

    verify(fieldApi)
        .searchFields(eq(searchTerm), eq(statuses), argumentCaptor.capture(), requestPurposeArgCaptor.capture());

    var requestedParams = argumentCaptor.getValue().getFields();

    assertThat(queryResults).isEqualTo(fields);
    assertThat(requestedParams).containsExactly(
        entry("fieldId", null),
        entry("fieldName", null)
    );
    assertThat(requestPurposeArgCaptor.getValue())
        .extracting(RequestPurpose::purpose)
        .isEqualTo(purpose);
  }

  @Test
  void getFieldById() {
    var requestedFieldId = 34;
    var requestPurpose = "Test request purpose";
    var field = Field.newBuilder()
        .fieldId(requestedFieldId)
        .fieldName("Test field name")
        .build();
    var argumentCaptor = ArgumentCaptor.forClass(FieldProjectionRoot.class);

    when(fieldApi.findFieldById(eq(requestedFieldId), any(FieldProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Optional.of(field));

    var returnedField = fieldService.getFieldById(requestedFieldId, requestPurpose);

    verify(fieldApi)
        .findFieldById(eq(requestedFieldId), argumentCaptor.capture(), requestPurposeArgumentCaptor.capture());
    var requestedParams = argumentCaptor.getValue().getFields();
    assertThat(requestedParams).containsExactly(
        entry("fieldId", null),
        entry("fieldName", null)
    );
    assertThat(returnedField).contains(field);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(requestPurpose);
  }

  @Test
  void doesFieldExist_doesExist_assertTrue() {
    var existingFieldId = 1;
    var purpose = "Validate that Field exists for SCAP project details";
    when(fieldApi.findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Optional.of(fields.get(0)));

    assertTrue(fieldService.doesFieldExist(1));

    verify(fieldApi).findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), requestPurposeArgumentCaptor.capture());
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
  }

  @Test
  void doesFieldExist_doesNotExist_assertFalse() {
    var existingFieldId = 3;
    var purpose = "Validate that Field exists for SCAP project details";
    when(fieldApi.findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Optional.empty());

    assertFalse(fieldService.doesFieldExist(3));

    verify(fieldApi).findFieldById(eq(existingFieldId), any(FieldProjectionRoot.class), requestPurposeArgumentCaptor.capture());
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
  }

  @Test
  void getFieldSearchResults() {
    var fieldsSearchResult = fieldService.getFieldsSearchResult(fields);

    assertThat(fieldsSearchResult.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(fields.get(0).getFieldId().toString(), fields.get(0).getFieldName()),
        tuple(fields.get(1).getFieldId().toString(), fields.get(1).getFieldName())
    );
  }

  @Test
  void getFieldsByIds() {
    var fieldIds = fields.stream().map(Field::getFieldId).toList();
    var purpose = "Test request purpose";
    when(fieldApi.getFieldsByIds(
        eq(fieldIds),
        eq(FieldService.FIELDS_PROJECTION_ROOT),
        any(RequestPurpose.class))
    ).thenReturn(fields);

    var fieldsByIds = fieldService.getFieldsByIds(fieldIds, purpose);

    verify(fieldApi).getFieldsByIds(
        eq(fieldIds),
        eq(FieldService.FIELDS_PROJECTION_ROOT),
        requestPurposeArgumentCaptor.capture()
    );

    assertThat(fieldsByIds).isEqualTo(fields);
    assertThat(requestPurposeArgumentCaptor.getValue())
        .extracting(RequestPurpose::purpose)
        .isEqualTo(purpose);
  }
}
