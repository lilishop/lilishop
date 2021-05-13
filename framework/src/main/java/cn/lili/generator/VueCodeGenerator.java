package cn.lili.generator;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.ResultMessage;
import cn.lili.generator.bean.Field;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.beetl.core.Configuration;
import org.beetl.core.GroupTemplate;
import org.beetl.core.Template;
import org.beetl.core.resource.ClasspathResourceLoader;
import org.elasticsearch.common.util.ArrayUtils;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * @author lili
 */
@Slf4j
@RestController
@Api(tags = "Vue代码生成")
@RequestMapping(value = "/manager/generate")
public class VueCodeGenerator {

    @RequestMapping(value = "/table/{vueName}/{rowNum}", method = RequestMethod.POST)
    @ApiOperation(value = "增删改表格生成")
    public ResultMessage<Object> generateTable(@PathVariable String vueName,
                                               @PathVariable Integer rowNum,
                                               @RequestBody List<Field> fields) throws IOException {
        String result = generate("table.btl", vueName, rowNum, fields);
        return ResultUtil.data(result);
    }

    @RequestMapping(value = "/tree/{vueName}/{rowNum}", method = RequestMethod.POST)
    @ApiOperation(value = "树形结构生成")
    public ResultMessage<Object> generateTree(@PathVariable String vueName,
                                              @PathVariable Integer rowNum,
                                              @RequestBody List<Field> fields) throws IOException {


        String result = generate("tree.btl", vueName, rowNum, fields);
        return ResultUtil.data(result);
    }

    @RequestMapping(value = "/getEntityData/{path}", method = RequestMethod.GET)
    @ApiOperation(value = "通过实体类生成Vue代码Json数据")
    public ResultMessage<Object> getEntityData(@PathVariable String path) {

        String result = "";
        try {
            result = generateClassData(path);
        } catch (Exception e) {
            return ResultUtil.error(ResultCode.ERROR);
        }
        return ResultUtil.data(result);
    }

    /**
     * 生成代码
     *
     * @param template 模版名称
     * @param vueName  表单名称
     * @param rowNum   树形表格所需参数，一行几列
     * @param fields
     * @return
     * @throws IOException
     */
    public String generate(String template, String vueName, Integer rowNum, List<Field> fields) throws IOException {

        // 模板路径
        ClasspathResourceLoader resourceLoader = new ClasspathResourceLoader("/templates/vue/");
        Configuration cfg = Configuration.defaultConfiguration();
        GroupTemplate gt = new GroupTemplate(resourceLoader, cfg);

        Template tableTemplate = gt.getTemplate(template);
        // 排序
        Collections.sort(fields, Comparator.comparing(Field::getSortOrder));
        // 绑定变量
        tableTemplate.binding("vueName", vueName);
        tableTemplate.binding("fields", fields);
        // 判断有无upload和日期范围搜索
        Boolean upload = false;
        for (Field f : fields) {
            if ("upload".equals(f.getType())) {
                upload = true;
            }
        }
        tableTemplate.binding("upload", upload);
        if ("table.btl".equals(template)) {
            // 判断有无upload和日期范围搜索
            Boolean daterangeSearch = false;
            for (Field f : fields) {
                if (f.getSearchable() && "daterange".equals(f.getSearchType())) {
                    daterangeSearch = true;
                }
            }
            tableTemplate.binding("daterangeSearch", daterangeSearch);
            // 统计搜索栏个数 判断是否隐藏搜索栏
            Boolean hideSearch = false;
            List<Field> firstTwo = new ArrayList<>();
            List<Field> rest = new ArrayList<>();
            Integer count = 0;
            for (Field f : fields) {
                if (f.getSearchable()) {
                    count++;
                    if (count <= 2) {
                        firstTwo.add(f);
                    } else {
                        rest.add(f);
                    }
                }
            }
            if (count >= 4) {
                hideSearch = true;
                tableTemplate.binding("firstTwo", firstTwo);
                tableTemplate.binding("rest", rest);
            }
            tableTemplate.binding("searchSize", count);
            tableTemplate.binding("hideSearch", hideSearch);
            // 获取默认排序字段
            String defaultSort = "", defaultSortType = "";
            for (Field f : fields) {
                if (f.getDefaultSort()) {
                    defaultSort = f.getField();
                    defaultSortType = f.getDefaultSortType();
                    break;
                }
            }
            tableTemplate.binding("defaultSort", defaultSort);
            tableTemplate.binding("defaultSortType", defaultSortType);
        }
        // 一行几列
        tableTemplate.binding("rowNum", rowNum);
        if (rowNum == 1) {
            tableTemplate.binding("modalWidth", 500);
            tableTemplate.binding("width", "100%");
            tableTemplate.binding("editWidth", "100%");
            tableTemplate.binding("itemWidth", "");
            tableTemplate.binding("span", "9");
        } else if (rowNum == 2) {
            tableTemplate.binding("modalWidth", 770);
            tableTemplate.binding("width", "250px");
            tableTemplate.binding("editWidth", "250px");
            tableTemplate.binding("itemWidth", "350px");
            tableTemplate.binding("span", "17");
        } else if (rowNum == 3) {
            tableTemplate.binding("modalWidth", 980);
            tableTemplate.binding("width", "200px");
            tableTemplate.binding("editWidth", "200px");
            tableTemplate.binding("itemWidth", "300px");
            tableTemplate.binding("span", "17");
        } else if (rowNum == 4) {
            tableTemplate.binding("modalWidth", 1130);
            tableTemplate.binding("width", "160px");
            tableTemplate.binding("editWidth", "160px");
            tableTemplate.binding("itemWidth", "260px");
            tableTemplate.binding("span", "17");
        } else {
            throw new ServiceException("rowNum仅支持数字1-4");
        }
        // 生成代码
        String result = tableTemplate.render();
        return result;
    }

    /**
     * 生成代码数据
     *
     * @param path
     * @return
     * @throws Exception
     */
    public String generateClassData(String path) throws Exception {

        Class c = Class.forName(path);
        Object obj = c.newInstance();
        java.lang.reflect.Field[] fields = ArrayUtils.concat(obj.getClass().getDeclaredFields(), obj.getClass().getSuperclass().getDeclaredFields(), java.lang.reflect.Field.class);

        //下标
        int index = 0;

        StringBuffer fieldsData = new StringBuffer();
        for (java.lang.reflect.Field field : fields) {
            index++;
            field.setAccessible(true);
            // 字段名
            String fieldName = field.getName();
            String fieldType = field.getType().getName();
            // 序列化id，不参与表单
            if ("serialVersionUID".equals(fieldName)) {
                continue;
            }

            // 获得字段注解
            ApiModelProperty myFieldAnnotation = field.getAnnotation(ApiModelProperty.class);
            //表单名称
            String formName = fieldName;
            if (myFieldAnnotation != null) {
                formName = myFieldAnnotation.value();
            }
            formName = StringUtils.isEmpty(formName) ? fieldName : formName;

            //默认类型
            String type = "text";
            String searchType = "text";
            // 日期字段特殊处理,其他一律按 字符串界面处理
            if (fieldType == "java.lang.Date" || fieldType == "java.util.Date" || fieldType == "Date") {
                type = "date";
                searchType = "daterange";
            }
            //表单子属性
            String formItem = "{" +
                    "\"sortOrder\":" + index + "," +
                    "\"field\":\"" + fieldName + "\"," +
                    "\"name\":\"" + formName + "\"," +
                    "\"level\":\"2\"," +
                    "\"tableShow\":true," +
                    "\"editable\":true," +
                    "\"type\":\"" + type + "\"," +
                    "\"searchType\":\"" + searchType + "\"," +
                    "\"searchLevel\":\"2\"," +
                    "\"validate\":false," +
                    "\"searchable\":true," +
                    "\"sortable\":false," +
                    "\"defaultSort\":false," +
                    "\"defaultSortType\":\"desc\"},";
            fieldsData.append(formItem);
        }
        String start = "{\"data\":[";
        String end = "]}";
        String json = start + fieldsData.substring(0, fieldsData.length()-1) + end;
        return json;
    }
}
