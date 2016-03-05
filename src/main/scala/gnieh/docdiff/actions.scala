/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh.docdiff

sealed trait Action[Annotation]

final case class Insert[Annotation](node: TextualConstituent[Annotation], father: InternalConstituent[Annotation], position: Int) extends Action[Annotation]

final case class Delete[Annotation](node: TextualConstituent[Annotation]) extends Action[Annotation]

final case class Update[Annotation](node: TextualConstituent[Annotation], element: TextualConstituent[Annotation]) extends Action[Annotation]

final case class Move[Annotation](node: TextualConstituent[Annotation], father: InternalConstituent[Annotation], position: Int) extends Action[Annotation]

